/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 by Fred Dushin <fred@dushin.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include "atomvm_socket.h"

#include <context.h>
#include <defaultatoms.h>
#include <dictionary.h>
#include <globalcontext.h>
#include <interop.h>
#include <list.h>
#include <mailbox.h>
#include <scheduler.h>
#include <term.h>
#include <utils.h>
#include <port.h>

// #define ENABLE_TRACE
#include <trace.h>

#include <esp_log.h>
#include <esp32_sys.h>

#include <sys/socket.h>
#include <errno.h>
#include <fcntl.h>

#include <freertos/FreeRTOS.h>
#include <freertos/task.h>
#include <freertos/queue.h>

#define TAG "atomvm_socket"


static const char *const domain_atom = "\x6" "domain";
static const char *const inet_atom = "\x4" "inet";
static const char *const type_atom = "\x4" "type";
static const char *const stream_atom = "\x6" "stream";
static const char *const dgram_atom = "\x5" "dgram";
static const char *const close_atom = "\x5" "close";
static const char *const bind_atom = "\x4" "bind";
static const char *const any_atom = "\x3" "any";
static const char *const loopback_atom = "\x8" "loopback";
static const char *const port_atom = "\x4" "port";
static const char *const addr_atom = "\x4" "addr";
static const char *const listen_atom = "\x6" "listen";
static const char *const accept_atom = "\x6" "accept";
static const char *const sockname_atom = "\x8" "sockname";
static const char *const peername_atom = "\x8" "peername";
static const char *const recv_atom = "\x4" "recv";
static const char *const send_atom = "\x4" "send";
static const char *const atomvm_socket_atom = "\xD" "atomvm_socket";
static const char *const closed_atom = "\x6" "closed";

#define DOMAIN_ATOM make_atom(global, domain_atom)
#define INET_ATOM make_atom(global, inet_atom)
#define TYPE_ATOM make_atom(global, type_atom)
#define STREAM_ATOM make_atom(global, stream_atom)
#define DGRAM_ATOM make_atom(global, dgram_atom)
#define CLOSE_ATOM make_atom(global, close_atom)
#define BIND_ATOM make_atom(global, bind_atom)
#define ANY_ATOM make_atom(global, any_atom)
#define LOOPBACK_ATOM make_atom(global, loopback_atom)
#define PORT_ATOM make_atom(global, port_atom)
#define ADDR_ATOM make_atom(global, addr_atom)
#define LISTEN_ATOM make_atom(global, listen_atom)
#define ACCEPT_ATOM make_atom(global, accept_atom)
#define SOCKNAME_ATOM make_atom(global, sockname_atom)
#define PEERNAME_ATOM make_atom(global, peername_atom)
#define RECV_ATOM make_atom(global, recv_atom)
#define SEND_ATOM make_atom(global, send_atom)
#define ATOMVM_SOCKET_ATOM make_atom(global, atomvm_socket_atom)
#define CLOSED_ATOM make_atom(global, closed_atom)

#define DEFAULT_BUFFER_SIZE 512
#define SELECT_QUEUE_SIZE 32
#define SELECT_TASK_STACK 5000
#define SELECT_TASK_SLEEP_MS 50
#define SELECT_TASK_TIMEOUT_MS 0

#define MIN(A, B) (((A) < (B)) ? (A) : (B))

static xQueueHandle select_queue;

enum SelectTaskOperation {
    op_ADD,
    op_REMOVE
};

struct SelectTaskMessage {
    enum SelectTaskOperation op;
    Context *ctx;
    int fd;
};

struct FDListElement {
    struct ListHead head;
    Context *ctx;
    int fd;
};

enum SocketState {
    idle,
    waiting_accept,
    waiting_recv
};

struct AsyncResponseData {
    Context *ctx;
    term pid;
    uint64_t ref_ticks;
};

struct SocketData {
    EventListener event_listener;
    enum SocketState socket_state;
    struct AsyncResponseData async_response_data;
    int fd;
    size_t buffer_size;
};

static void consume_mailbox(Context *ctx);
static Context *create_context(GlobalContext *global, int fd);

//
// select RTOS task
//

static void update_fds(struct ListHead *fds, struct SelectTaskMessage *msg)
{
    switch(msg->op) {
        case op_ADD: {
            TRACE("Got op_ADD message for 0x%p,%i\n", msg->ctx, msg->fd);
            struct FDListElement *elt = malloc(sizeof(struct FDListElement));
            elt->ctx = msg->ctx;
            elt->fd = msg->fd;
            list_append(fds, (struct ListHead *) elt);
            break;
        }
        case op_REMOVE: {
            struct ListHead *item, *tmp;
            TRACE("Got op_REMOVE message for 0x%p,%i\n", msg->ctx, msg->fd);
            MUTABLE_LIST_FOR_EACH (item, tmp, fds) {
                struct FDListElement *elt = (struct FDListElement *) item;
                if (elt->fd == msg->fd) {
                    TRACE("Removing fd=%i\n", elt->fd);
                    list_remove(item);
                    free(item);
                }
            }
            break;
        }
        default:
            break;
    }
}

static void maybe_select(struct ListHead *fds)
{
    if (!list_is_empty(fds)) {
        // TRACE("fds non-empty\n");

        fd_set read_fds;
        FD_ZERO(&read_fds);

        struct ListHead *item;
        int fd_max = 0;
        LIST_FOR_EACH (item, fds) {
            struct FDListElement *elt = (struct FDListElement *) item;
            FD_SET(elt->fd, &read_fds);
            // TRACE("Setting fd %i\n", elt->fd);
            if (elt->fd > fd_max) {
                fd_max = elt->fd;
            }
        }

        struct timeval tv;
        tv.tv_sec = 0;
        tv.tv_usec = SELECT_TASK_TIMEOUT_MS * 1000;
        int res = select(fd_max + 1, &read_fds, NULL, NULL, &tv);
        // TRACE("select res=%i\n", res);

        if (res == -1) {
            ESP_LOGW(TAG, "Error calling select.");
        } else if (res == 0) {
            // TRACE("No socket fds are ready.\n");
        } else {
            LIST_FOR_EACH (item, fds) {
                struct FDListElement *elt = (struct FDListElement *) item;
                if (FD_ISSET(elt->fd, &read_fds)) {
                    TRACE("socket 0x%p,%i ready for read\n", elt->ctx, elt->fd);
                    BaseType_t result = xQueueSendToBack(event_queue, &elt->ctx, 0);
                    if (result != pdPASS) {
                        ESP_LOGW(TAG, "Unable to push context onto event queue.");
                    } else {
                        TRACE("Pushed context 0x%p,%i onto event_queue\n", elt->ctx, elt->fd);
                    }
                } else {
                    // TRACE("socket fd %i idle\n", elt->fd);
                }
            }
        }
    }
}

void select_task(void *args)
{
    TRACE("Entered select_task\n");
    xQueueHandle select_queue = (xQueueHandle) args;

    struct ListHead fds;
    list_init(&fds);

    for( ;; ) {
        // TRACE("select task running\n");

        struct SelectTaskMessage msg;
        BaseType_t res = xQueueReceive(select_queue, &msg, 0);
        if (res == pdPASS) {
            update_fds(&fds, &msg);
            #ifdef ENABLE_TRACE
                TRACE("[");
                struct ListHead *item;
                LIST_FOR_EACH (item, &fds) {
                    struct FDListElement *elt = (struct FDListElement *) item;
                    TRACE("%i,", elt->fd);
                }
                TRACE("]\n");
            #endif
        } else if (res == errQUEUE_EMPTY) {
            // TRACE("queue is empty!\n");
        }

        maybe_select(&fds);

        vTaskDelay(SELECT_TASK_SLEEP_MS / portTICK_PERIOD_MS);
    }
    vTaskDelete(NULL);
}

//
// socket operations
//

static inline term make_atom(GlobalContext *global, AtomString string)
{
    int global_atom_index = globalcontext_insert_atom(global, string);
    return term_from_atom_index(global_atom_index);
}

static void send_message(term pid, term message, GlobalContext *global)
{
    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(global, local_process_id);
    mailbox_send(target, message);
}

static uint32_t socket_tuple_to_addr(term addr_tuple)
{
    return ((term_to_int32(term_get_tuple_element(addr_tuple, 0)) & 0xFF) << 24)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 1)) & 0xFF) << 16)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 2)) & 0xFF) << 8)
        | (term_to_int32(term_get_tuple_element(addr_tuple, 3)) & 0xFF);
}

static term socket_tuple_from_addr(Context *ctx, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int32((addr >> 24) & 0xFF);
    terms[1] = term_from_int32((addr >> 16) & 0xFF);
    terms[2] = term_from_int32((addr >> 8) & 0xFF);
    terms[3] = term_from_int32(addr & 0xFF);

    return port_create_tuple_n(ctx, 4, terms);
}

static void send_reply(Context *ctx, term pid, uint64_t ref_ticks, term return_value)
{
    if (term_is_boxed(return_value)) {
        GlobalContext *global = ctx->global;
        dictionary_put(&ctx->dictionary, ctx, ATOMVM_SOCKET_ATOM, return_value);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
        } else {
            return_value = dictionary_get(&ctx->dictionary, ctx, ATOMVM_SOCKET_ATOM);
            term return_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(return_tuple, 0, term_from_ref_ticks(ref_ticks, ctx));
            term_put_tuple_element(return_tuple, 1, return_value);
            send_message(pid, return_tuple, ctx->global);
        }
        dictionary_erase(&ctx->dictionary, ctx, ATOMVM_SOCKET_ATOM);
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
        } else {
            term return_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(return_tuple, 0, term_from_ref_ticks(ref_ticks, ctx));
            term_put_tuple_element(return_tuple, 1, return_value);
            send_message(pid, return_tuple, ctx->global);
        }
    }
}

static void send_error_tuple(Context *ctx, term pid, uint64_t ref_ticks, term reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    } else {
        term error_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, reason);

        send_reply(ctx, pid, ref_ticks, error_tuple);
    }
}

static void close_socket(Context *ctx)
{
    struct SocketData *socket_data = ctx->platform_data;
    EventListener *listener = &socket_data->event_listener;

    struct SelectTaskMessage select_msg = {
        .op = op_REMOVE,
        .fd = socket_data->fd,
        .ctx = ctx
    };
    TRACE("Pushing REMOVE msg for 0x%p,%i onto select_queue\n", ctx, socket_data->fd);
    BaseType_t res = xQueueSendToBack(select_queue, &select_msg, 0);
    if (res != pdPASS) {
        ESP_LOGW(TAG, "Unable to push remove to select_queue.");
    }
    list_remove(&listener->listeners_list_head);

    int sres = close(socket_data->fd);
    if (sres != 0) {
        ESP_LOGW(TAG, "Unable to close socket.");
    }
    TRACE("Destroying context 0x%p with (removed) EventListener 0x%p\n", ctx, listener);

    free(socket_data);
    scheduler_terminate(ctx);
}

static void do_close(Context *ctx, term msg)
{
    TRACE("do_close\n");
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    send_reply(ctx, pid, ref_ticks, OK_ATOM);

    // notify any procs that might be stuck in accept or recv if the parent socket was closed
    if (socket_data->socket_state != idle) {
        GlobalContext *global = ctx->global;
        EventListener *listener = &socket_data->event_listener;
        struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;
        TRACE("Notifying pid %i that socket has closed\n", async_response_data->pid);
        send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, CLOSED_ATOM);
    }

    close_socket(ctx);
}

static void do_bind(Context *ctx, term msg, term cmd)
{
    TRACE("do_bind\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    term sockaddr = term_get_tuple_element(cmd, 1);

    struct sockaddr_in serveraddr;
    memset(&serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;

    if (sockaddr == ANY_ATOM) {
        serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
    } else if (sockaddr == LOOPBACK_ATOM) {
        serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    } else if (term_is_map(sockaddr)) {
        term port = term_get_map_assoc_default(ctx, sockaddr, PORT_ATOM, term_from_int(0));
        serveraddr.sin_port = htons(term_to_int(port));
        term addr = term_get_map_assoc(ctx, sockaddr, ADDR_ATOM);
        if (addr == ANY_ATOM) {
            serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
        } else if (addr == LOOPBACK_ATOM) {
            serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        } else {
            serveraddr.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
        }
    }

    socklen_t address_len = sizeof(serveraddr);
    int res = bind(socket_data->fd, (struct sockaddr *) &serveraddr, address_len);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    }
    if (res != 0) {
        ESP_LOGE(TAG, "Unable to bind socket: res=%i.", res);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        send_reply(ctx, pid, ref_ticks, OK_ATOM);
    }
}

static void do_listen(Context *ctx, term msg, term cmd)
{
    TRACE("do_listen\n");
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    term backlog = term_get_tuple_element(cmd, 1);

    int res = listen(socket_data->fd, term_to_int(backlog));

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    }
    if (res != 0) {
        ESP_LOGE(TAG, "Unable to listen on socket: res=%i.", res);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        send_reply(ctx, pid, ref_ticks, OK_ATOM);
    }
}


static void accept_handler(EventListener *listener)
{
    TRACE("accept_handler\n");
    struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;
    Context *ctx = async_response_data->ctx;
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    int fd = accept(socket_data->fd, (struct sockaddr *) &clientaddr, &clientlen);
    if (fd == -1) {
        ESP_LOGE(TAG, "Unable to accept on socket %i.", socket_data->fd);
        send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, term_from_int(errno));
    } else {

        TRACE("atomvm_socket.accept_handler: accepted connection.  fd: %i\n", fd);

        Context *new_ctx = create_context(global, fd);
        scheduler_make_waiting(global, new_ctx);

        // {ok, Socket}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            send_message(async_response_data->pid, OUT_OF_MEMORY_ATOM, ctx->global);
        }
        term return_value = term_alloc_tuple(2, ctx);
        term new_pid = term_from_local_process_id(new_ctx->process_id);
        term_put_tuple_element(return_value, 0, OK_ATOM);
        term_put_tuple_element(return_value, 1, new_pid);

        send_reply(ctx, async_response_data->pid, async_response_data->ref_ticks, return_value);
    }
    socket_data->socket_state = idle;
    list_remove(&listener->listeners_list_head);
}

static void do_accept(Context *ctx, term msg)
{
    TRACE("do_accept\n");
    GlobalContext *global = ctx->global;
    struct ESP32PlatformData *platform = global->platform_data;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    EventListener *listener = &socket_data->event_listener;
    struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;

    async_response_data->ctx = ctx;
    async_response_data->pid = pid;
    async_response_data->ref_ticks = ref_ticks;
    socket_data->socket_state = waiting_accept;

    list_init(&listener->listeners_list_head);
    listener->handler = accept_handler;
    list_append(&platform->listeners, &listener->listeners_list_head);

    TRACE("EventListener 0x%p with sender=0x%p added to platform listeners for accept\n", listener, ctx);
}

static void do_sockname(Context *ctx, term msg)
{
    TRACE("do_sockname\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    int res = getsockname(socket_data->fd, (struct sockaddr *) &addr, &addrlen);

    if (res != 0) {
        ESP_LOGE(TAG, "Unable to getsockname: fd=%i res=%i.", socket_data->fd, res);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        // {ok, #{addr => {a,b,c,d}, port => integer()}}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
            send_message(pid, OUT_OF_MEMORY_ATOM, global);
        } else {
            term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
            term port_number = term_from_int(ntohs(addr.sin_port));

            term map = term_alloc_map(ctx, 2);
            term_set_map_assoc(map, 0, ADDR_ATOM, address);
            term_set_map_assoc(map, 1, PORT_ATOM, port_number);
            term return_value = port_create_tuple2(ctx, OK_ATOM, map);

            send_reply(ctx, pid, ref_ticks, return_value);
        }
    }
}

static void do_peername(Context *ctx, term msg)
{
    TRACE("do_peername\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    int res = getpeername(socket_data->fd, (struct sockaddr *) &addr, &addrlen);

    if (res != 0) {
        ESP_LOGE(TAG, "Unable to getpeername: fd=%i res=%i.", socket_data->fd, res);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        // {ok, #{addr => {a,b,c,d}, port => integer()}}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
            send_message(pid, OUT_OF_MEMORY_ATOM, global);
        } else {
            term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
            term port_number = term_from_int(ntohs(addr.sin_port));

            term map = term_alloc_map(ctx, 2);
            term_set_map_assoc(map, 0, ADDR_ATOM, address);
            term_set_map_assoc(map, 1, PORT_ATOM, port_number);
            term return_value = port_create_tuple2(ctx, OK_ATOM, map);

            send_reply(ctx, pid, ref_ticks, return_value);
        }
    }
}

static void recv_handler(EventListener *listener)
{
    TRACE("recv_handler\n");
    struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;
    Context *ctx = async_response_data->ctx;
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;
    //
    // allocate the receive buffer
    //
    ssize_t num_available_bytes = recvfrom(socket_data->fd, NULL, socket_data->buffer_size, MSG_PEEK | MSG_WAITALL, NULL, NULL);
    TRACE("%i bytes available.\n", num_available_bytes);
    if (num_available_bytes == 0) {
        ESP_LOGI(TAG, "Peer closed socket %i.", socket_data->fd);
        send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, CLOSED_ATOM);
        close_socket(ctx);
    } else {
        size_t buffer_size = MIN(num_available_bytes, socket_data->buffer_size);
        // {ok, Data}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_data_size_in_terms(buffer_size)) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Failed to allocate memory for recv buffer.");
            send_message(async_response_data->pid, OUT_OF_MEMORY_ATOM, global);
        } else {
            term data = term_create_uninitialized_binary(buffer_size, ctx);
            const char *buffer = term_binary_data(data);
            //
            // receive data on the socket
            //
            ssize_t len = recvfrom(socket_data->fd, (char *) buffer, buffer_size, MSG_WAITALL, NULL, NULL);
            if (len < 0) {
                ESP_LOGE(TAG, "Unable to read data on socket %i.", socket_data->fd);
                send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, term_from_int(errno));
            } else {
                TRACE("atomvm_socket.recv_handler: received data on fd: %i len=%u\n", socket_data->fd, len);

                // {ok, Data}
                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_data_size_in_terms(len)) != MEMORY_GC_OK)) {
                    ESP_LOGE(TAG, "Failed to allocate memory for error response.");
                    send_message(async_response_data->pid, OUT_OF_MEMORY_ATOM, global);
                } else {
                    term return_value = term_alloc_tuple(2, ctx);
                    term_put_tuple_element(return_value, 0, OK_ATOM);
                    term bin = term_from_literal_binary(buffer, len, ctx);
                    term_put_tuple_element(return_value, 1, bin);

                    send_reply(ctx, async_response_data->pid, async_response_data->ref_ticks, return_value);
                }
            }
        }
    }
    socket_data->socket_state = idle;
    list_remove(&listener->listeners_list_head);
}

static void do_recv(Context *ctx, term msg)
{
    TRACE("do_recv\n");
    GlobalContext *global = ctx->global;
    struct ESP32PlatformData *platform = global->platform_data;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    EventListener *listener = &socket_data->event_listener;
    struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;

    async_response_data->ctx = ctx;
    async_response_data->pid = pid;
    async_response_data->ref_ticks = ref_ticks;
    socket_data->socket_state = waiting_recv;

    list_init(&listener->listeners_list_head);
    listener->handler = recv_handler;
    list_append(&platform->listeners, &listener->listeners_list_head);

    TRACE("EventListener 0x%p with sender=0x%p added to platform listeners for recv\n", listener, ctx);
}

static void do_send(Context *ctx, term msg, term cmd)
{
    TRACE("do_send\n");
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);
    term data = term_get_tuple_element(cmd, 1);

    // {ok, RestData} | {error, Reason}
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    }

    char *buf = NULL;
    size_t len = 0;
    if (term_is_binary(data)) {
        buf = (char *) term_binary_data(data);
        len = term_binary_size(data);
    } else if (term_is_list(data)) {
        int ok;
        len = interop_iolist_size(data, &ok);
        if (UNLIKELY(!ok)) {
            send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
            return;
        }
        buf = malloc(len);
        if (UNLIKELY(!interop_write_iolist(data, buf))) {
            free(buf);
            send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
            return;
        }
    } else {
        send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
        return;
    }

    int sent_data = send(socket_data->fd, buf, len, 0);
    if (term_is_list(data)) {
        free(buf);
    }

    if (sent_data == -1) {
        ESP_LOGE(TAG, "Unable to send data: res=%i.", sent_data);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        term return_value = port_create_tuple2(ctx, OK_ATOM, term_nil());
        send_reply(ctx, pid, ref_ticks, return_value);
    }
}


    // TODO add support for setsockopt
    // int flag = 1;
    // int res = setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof(int));
    // if (res != 0) {
    //     ESP_LOGW(TAG, "Failed to set TCP_NODELAY.");
    // }



//
//
//

static void consume_mailbox(Context *ctx)
{
    TRACE("Processing mailbox message for context 0x%p\n", ctx);
    GlobalContext *global = ctx->global;
    while (!list_is_empty(&ctx->mailbox)) {
        Message *message = mailbox_dequeue(ctx);
        term msg = message->message;

        // TRACE("message: ");
        // #ifdef ENABLE_TRACE
        //     term_display(stdout, msg, ctx);
        // #endif
        // TRACE("\n");

        term cmd = term_get_tuple_element(msg, 2);

        if (cmd == CLOSE_ATOM) {
            do_close(ctx, msg);
        } else if (cmd == SOCKNAME_ATOM) {
            do_sockname(ctx, msg);
        } else if (cmd == PEERNAME_ATOM) {
            do_peername(ctx, msg);
        } else if (cmd == ACCEPT_ATOM) {
            do_accept(ctx, msg);
        } else if (cmd == RECV_ATOM) {
            do_recv(ctx, msg);
        } else if (term_is_tuple(cmd)) {
            term cmd_name = term_get_tuple_element(cmd, 0);
            if (cmd_name == BIND_ATOM) {
                do_bind(ctx, msg, cmd);
            } else if (cmd_name == LISTEN_ATOM) {
                do_listen(ctx, msg, cmd);
            } else if (cmd_name == SEND_ATOM) {
                do_send(ctx, msg, cmd);
            } else {
                TRACE("badarg\n");
            }
        }

        mailbox_destroy_message(message);
    }
}


//
// entrypoints
//


void atomvm_socket_init(GlobalContext *global)
{
    select_queue = xQueueCreate(SELECT_QUEUE_SIZE, sizeof(struct SelectTaskMessage));
    if (IS_NULL_PTR(select_queue)) {
        ESP_LOGE(TAG, "Failed to initialize select_queue.");
        AVM_ABORT();
    } else {
        BaseType_t res = xTaskCreate(select_task, "select_task", SELECT_TASK_STACK, select_queue, 1, NULL);
        if (res != pdPASS) {
            ESP_LOGE(TAG, "Failed to initialize select_task.  Error: %i", res);
            vQueueDelete(select_queue);
            AVM_ABORT();
        }
    }
    ESP_LOGI(TAG, "Initialized AtomVM socket.");
}

static inline int get_domain(GlobalContext *global, term domain_term)
{
    if (domain_term == INET_ATOM) {
        return PF_INET;
    } else {
        ESP_LOGW(TAG, "Unsupported domain.  Defaulting to inet.");
        return PF_INET;
    }
}

static inline int get_type(GlobalContext *global, term type_term)
{
    if (type_term == STREAM_ATOM) {
        return SOCK_STREAM;
    } else if (type_term == DGRAM_ATOM) {
        return SOCK_DGRAM;
    } else {
        ESP_LOGW(TAG, "Unsupported type.  Defaulting to stream.");
        return SOCK_STREAM;
    }
}

static Context *create_context(GlobalContext *global, int fd)
{
    // TODO Will we ever need to make socket non-blocking, if we are using accept?
    // if (fcntl(fd, F_SETFL, O_NONBLOCK) == -1) {
    //     close(fd);
    //     return NULL;
    // }

    struct SocketData *socket_data = malloc(sizeof(struct SocketData));
    if (IS_NULL_PTR(socket_data)) {
        ESP_LOGE(TAG, "Insufficient memory to allocate SocketData.");
        return NULL;
    }
    socket_data->fd = fd;
    socket_data->buffer_size = DEFAULT_BUFFER_SIZE;
    socket_data->event_listener.data = &socket_data->async_response_data;
    socket_data->socket_state = idle;

    Context *ctx = context_new(global);
    if (IS_NULL_PTR(ctx)) {
        ESP_LOGE(TAG, "Insufficient memory to allocate Context.");
        free(socket_data);
        return NULL;
    }
    ctx->native_handler = consume_mailbox;
    ctx->platform_data = socket_data;
    socket_data->event_listener.sender = ctx;
    TRACE("Created Context 0x%p with EventListener 0x%p\n", ctx, &socket_data->event_listener);

    struct SelectTaskMessage msg = {
        .op = op_ADD,
        .fd = fd,
        .ctx = ctx
    };
    TRACE("Pushing context 0x%p,fd=%i onto select_queue\n", ctx, fd);
    BaseType_t res = xQueueSendToBack(select_queue, &msg, 0);
    if (res != pdPASS) {
        ESP_LOGW(TAG, "Unable to push fd %i to select queue.", fd);
    }

    return ctx;
}


Context *atomvm_socket_create_port(GlobalContext *global, term opts)
{
    term domain_term = interop_proplist_get_value(opts, DOMAIN_ATOM);
    term type_term = interop_proplist_get_value(opts, TYPE_ATOM);

    int fd = socket(get_domain(global, domain_term), get_type(global, type_term), IPPROTO_IP);
    if (fd == -1) {
        ESP_LOGE(TAG, "Failed to initialize socket.");
        return NULL;
    }

    return create_context(global, fd);
}
