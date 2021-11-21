//
// Copyright (c) 2021 dushin.net
// All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include "atomvm_mqtt_client.h"

#include <atom.h>
#include <bif.h>
#include <context.h>
#include <debug.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <module.h>
#include <port.h>
#include <scheduler.h>
#include <term.h>
#include <utils.h>

#include <esp_log.h>
#include <mqtt_client.h>

// #define ENABLE_TRACE
#include <trace.h>


#define TAG "atomvm_mqtt"

static const char *const mqtt_atom =            "\x4" "mqtt";
static const char *const stop_atom =            "\x4" "stop";
static const char *const receiver_atom =        "\x8" "receiver";
static const char *const url =                  "\x3" "url";
static const char *const connected_atom =       "\x9" "connected";
static const char *const disconnected_atom =    "\xC" "disconnected";
static const char *const disconnect_atom =      "\xA" "disconnect";
static const char *const reconnect_atom =       "\x9" "reconnect";
static const char *const published_atom =       "\x9" "published";
static const char *const subscribed_atom =      "\xA" "subscribed";
static const char *const unsubscribed_atom =    "\xC" "unsubscribed";
static const char *const data_atom =            "\x4" "data";
static const char *const publish_atom =         "\x7" "publish";
static const char *const subscribe_atom =       "\x9" "subscribe";
static const char *const unsubscribe_atom =     "\xB" "unsubscribe";

// error codes
static const char *const esp_tls_atom =                 "\x07" "esp_tls";
static const char *const connection_refused_atom =      "\x12" "connection_refused";
static const char *const connection_accepted_atom =     "\x13" "connection_accepted";
static const char *const protocol_atom =                "\x08" "protocol";
static const char *const id_rejected_atom =             "\x0B" "id_rejected";
static const char *const server_unavailable_atom =      "\x12" "server_unavailable";
static const char *const bad_username_atom =            "\x0C" "bad_username";
static const char *const not_authorized_atom =          "\x0E" "not_authorized";
//                                                             0123456789ABCDEF0123456789ABCDEF
//                                                             0               1


static void consume_mailbox(Context *ctx);
static term do_publish(Context *ctx, term topic, term data, term qos, term retain);
static term do_subscribe(Context *ctx, term topic, term qos);
static term do_unsubscribe(Context *ctx, term topic);
static void do_stop(Context *ctx);
static term do_disconnect(Context *ctx);
static term do_reconnect(Context *ctx);
static esp_err_t mqtt_event_handler(esp_mqtt_event_handle_t event);
static term make_atom(GlobalContext *global, const char *string);
static term error_type_to_atom(Context *ctx, esp_mqtt_error_type_t error_type);
static term connect_return_code_to_atom(Context *ctx, esp_mqtt_connect_return_code_t connect_return_code);
static term create_tuple4(Context *ctx, term a, term b, term c, term d);
static term create_tuple5(Context *ctx, term a, term b, term c, term d, term e);

struct platform_data {
    esp_mqtt_client_handle_t client;
    term receiver;
};

void atomvm_mqtt_client_init(GlobalContext *global)
{
    esp_log_level_set("MQTT_CLIENT", ESP_LOG_VERBOSE);
}

Context *atomvm_mqtt_client_create_port(GlobalContext *global, term opts)
{
    term receiver = interop_proplist_get_value(opts, make_atom(global, receiver_atom));
    term url_term = interop_proplist_get_value(opts, make_atom(global, url));

    int ok;
    char *url_str = interop_term_to_string(url_term, &ok);
    if (!ok) {
        ESP_LOGE(TAG, "Error: url is not a proper string or binary.");
        return NULL;
    }
    if (UNLIKELY(IS_NULL_PTR(url_str))) {
        ESP_LOGE(TAG, "Error: Unable to allocate url string.");
        return NULL;
    }

    Context *ctx = context_new(global);
    ctx->native_handler = consume_mailbox;

    esp_mqtt_client_config_t mqtt_cfg = {
        .uri = url_str,
        .event_handle = mqtt_event_handler,
        .user_context = (void *) ctx
    };
    esp_mqtt_client_handle_t client = esp_mqtt_client_init(&mqtt_cfg);
    free(url_str);
    if (UNLIKELY(IS_NULL_PTR(client))) {
        ESP_LOGE(TAG, "Error: Unable to initialize MQTT client.\n");
        return NULL;
    }
    esp_err_t err = esp_mqtt_client_start(client);
    if (err != ESP_OK) {
        context_destroy(ctx);
        ESP_LOGE(TAG, "Error: Unable to start MQTT client.  Error: %i.\n", err);
        return NULL;
    }

    struct platform_data *plfdat = malloc(sizeof(struct platform_data));
    plfdat->client = client;
    plfdat->receiver = receiver;
    ctx->platform_data = plfdat;

    TRACE(TAG ": MQTT started.\n");
    return ctx;
}


static void consume_mailbox(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);
    term req = term_get_tuple_element(msg, 2);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    term ret = ERROR_ATOM;
    if (term_is_atom(req)) {
        if (req == context_make_atom(ctx, stop_atom)) {
            do_stop(ctx);
            return;
        } else if (req == context_make_atom(ctx, disconnect_atom)) {
            ret = do_disconnect(ctx);
        } else if (req == context_make_atom(ctx, reconnect_atom)) {
            ret = do_reconnect(ctx);
        }
    }
    else if (term_is_tuple(req)) {
        term cmd = term_get_tuple_element(req, 0);
        if (cmd == context_make_atom(ctx, publish_atom)) {
            term topic = term_get_tuple_element(req, 1);
            term data = term_get_tuple_element(req, 2);
            term qos = term_get_tuple_element(req, 3);
            term retain = term_get_tuple_element(req, 4);
            ret = do_publish(ctx, topic, data, qos, retain);
        } else if (cmd == context_make_atom(ctx, subscribe_atom)) {
            term topic = term_get_tuple_element(req, 1);
            term qos = term_get_tuple_element(req, 2);
            ret = do_subscribe(ctx, topic, qos);
        } else if (cmd == context_make_atom(ctx, unsubscribe_atom)) {
            term topic = term_get_tuple_element(req, 1);
            ret = do_unsubscribe(ctx, topic);
        }
    }

    mailbox_destroy_message(message);

    if (UNLIKELY(memory_ensure_free(ctx, 3 + 2) != MEMORY_GC_OK)) {
        mailbox_send(target, MEMORY_ATOM);
    } else {
        term ret_msg = port_create_tuple2(ctx, term_from_ref_ticks(ref_ticks, ctx), ret);
        mailbox_send(target, ret_msg);
    }
}


static term do_publish(Context *ctx, term topic, term data, term qos, term retain)
{
    struct platform_data *plfdat = (struct platform_data *) ctx->platform_data;
    esp_mqtt_client_handle_t client = plfdat->client;

    int ok;
    char *topic_str = interop_term_to_string(topic, &ok);
    if (!ok) {
        return BADARG_ATOM;
    }

    TRACE(TAG ": do_publish topic=%s\n", topic_str);
    int msg_id = esp_mqtt_client_publish(
        client,
        topic_str,
        term_binary_data(data),
        term_binary_size(data),
        term_to_int(qos),
        retain == TRUE_ATOM ? 1 : 0
    );
    free(topic_str);

    if (msg_id == -1) {
        ESP_LOGE(TAG, "Error: unable to publish to topic.\n");
        return ERROR_ATOM;
    }

    return term_from_int(msg_id);
}


static term do_subscribe(Context *ctx, term topic, term qos)
{
    struct platform_data *plfdat = (struct platform_data *) ctx->platform_data;
    esp_mqtt_client_handle_t client = plfdat->client;

    int ok;
    char *topic_str = interop_term_to_string(topic, &ok);
    if (!ok) {
        return BADARG_ATOM;
    }

    TRACE(TAG ": do_subscribe topic=%s\n", topic_str);
    int msg_id = esp_mqtt_client_subscribe(
        client,
        topic_str,
        term_to_int(qos)
    );
    free(topic_str);

    if (msg_id == -1) {
        ESP_LOGE(TAG, "Error: unable to subscribe to topic.\n");
        return ERROR_ATOM;
    }

    return term_from_int(msg_id);
}


static term do_unsubscribe(Context *ctx, term topic)
{
    struct platform_data *plfdat = (struct platform_data *) ctx->platform_data;
    esp_mqtt_client_handle_t client = plfdat->client;

    int ok;
    char *topic_str = interop_term_to_string(topic, &ok);
    if (!ok) {
        return BADARG_ATOM;
    }

    TRACE(TAG ": do_unsubscribe topic=%s\n", topic_str);
    int msg_id = esp_mqtt_client_unsubscribe(
        client,
        topic_str
    );
    free(topic_str);

    if (msg_id == -1) {
        ESP_LOGE(TAG, "Error: unable to unsubscribe from topic.\n");
        return ERROR_ATOM;
    }

    return term_from_int(msg_id);
}


static void do_stop(Context *ctx)
{
    struct platform_data *plfdat = (struct platform_data *) ctx->platform_data;
    esp_mqtt_client_handle_t client = plfdat->client;

    TRACE(TAG ": do_stop\n");
    esp_mqtt_client_stop(client);
    esp_mqtt_client_destroy(client);
    scheduler_terminate(ctx);
    free(plfdat);
}


static term do_disconnect(Context *ctx)
{
    struct platform_data *plfdat = (struct platform_data *) ctx->platform_data;
    esp_mqtt_client_handle_t client = plfdat->client;

    TRACE(TAG ": do_disconnect\n");
    esp_err_t status = esp_mqtt_client_disconnect(client);

    if (status == ESP_OK) {
        ESP_LOGE(TAG, "Error: unable to disconnect from MQTT Broker.\n");
        return ERROR_ATOM;
    }

    return OK_ATOM;
}


static term do_reconnect(Context *ctx)
{
    struct platform_data *plfdat = (struct platform_data *) ctx->platform_data;
    esp_mqtt_client_handle_t client = plfdat->client;

    TRACE(TAG ": do_reconnect\n");
    esp_err_t status = esp_mqtt_client_reconnect(client);

    if (status == ESP_OK) {
        ESP_LOGE(TAG, "Error: unable to reconnect to MQTT Broker.\n");
        return ERROR_ATOM;
    }

    return OK_ATOM;
}


static esp_err_t mqtt_event_handler(esp_mqtt_event_handle_t event)
{
    Context *ctx = (Context *) event->user_context;
    struct platform_data *plfdat = (struct platform_data *) ctx->platform_data;
    int pid = term_to_local_process_id(plfdat->receiver);
    Context *target = globalcontext_get_process(ctx->global, pid);

    switch (event->event_id) {
        case MQTT_EVENT_CONNECTED: {
            TRACE(TAG ": MQTT_EVENT_CONNECTED\n");
            if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
                mailbox_send(target, MEMORY_ATOM);
                return ESP_OK;
            }
            term msg = port_create_tuple2(ctx,
                context_make_atom(ctx, mqtt_atom),
                context_make_atom(ctx, connected_atom)
            );
            mailbox_send(target, msg);
            break;
        }
        case MQTT_EVENT_DISCONNECTED: {
            TRACE(TAG ": MQTT_EVENT_DISCONNECTED\n");
            if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
                mailbox_send(target, MEMORY_ATOM);
                return ESP_OK;
            }
            term msg = port_create_tuple2(ctx,
                context_make_atom(ctx, mqtt_atom),
                context_make_atom(ctx, disconnected_atom)
            );
            mailbox_send(target, msg);
            break;
        }
        case MQTT_EVENT_SUBSCRIBED: {
            TRACE(TAG ": MQTT_EVENT_SUBSCRIBED, msg_id=%d\n", event->msg_id);
            if (UNLIKELY(memory_ensure_free(ctx, 4) != MEMORY_GC_OK)) {
                mailbox_send(target, MEMORY_ATOM);
                return ESP_OK;
            }
            term msg = port_create_tuple3(ctx,
                context_make_atom(ctx, mqtt_atom),
                context_make_atom(ctx, subscribed_atom),
                term_from_int(event->msg_id)
            );
            mailbox_send(target, msg);
            break;
        }
        case MQTT_EVENT_UNSUBSCRIBED: {
            TRACE(TAG ": MQTT_EVENT_UNSUBSCRIBED, msg_id=%d\n", event->msg_id);
            if (UNLIKELY(memory_ensure_free(ctx, 4) != MEMORY_GC_OK)) {
                mailbox_send(target, MEMORY_ATOM);
                return ESP_OK;
            }
            term msg = port_create_tuple3(ctx,
                context_make_atom(ctx, mqtt_atom),
                context_make_atom(ctx, unsubscribed_atom),
                term_from_int(event->msg_id)
            );
            mailbox_send(target, msg);
            break;
        }
        case MQTT_EVENT_PUBLISHED: {
            TRACE(TAG ": MQTT_EVENT_PUBLISHED, msg_id=%d\n", event->msg_id);
            if (UNLIKELY(memory_ensure_free(ctx, 4) != MEMORY_GC_OK)) {
                mailbox_send(target, MEMORY_ATOM);
                return ESP_OK;
            }
            term msg = port_create_tuple3(ctx,
                context_make_atom(ctx, mqtt_atom),
                context_make_atom(ctx, published_atom),
                term_from_int(event->msg_id)
            );
            mailbox_send(target, msg);
            break;
        }
        case MQTT_EVENT_DATA: {
            TRACE(TAG ": MQTT_EVENT_DATA, msg_id=%d\n", event->msg_id);
            TRACE(TAG ": TOPIC=%.*s\n", event->topic_len, event->topic);
            TRACE(TAG ": DATA=%.*s\n", event->data_len, event->data);
            int topic_size = term_binary_data_size_in_terms(event->topic_len);
            int data_size = term_binary_data_size_in_terms(event->data_len);
            if (UNLIKELY(memory_ensure_free(ctx, 5 + topic_size + data_size) != MEMORY_GC_OK)) {
                mailbox_send(target, MEMORY_ATOM);
                return ESP_OK;
            }
            term topic = term_from_literal_binary(event->topic, event->topic_len, ctx);
            term data = term_from_literal_binary(event->data, event->data_len, ctx);
            term msg = create_tuple4(ctx,
                context_make_atom(ctx, mqtt_atom),
                context_make_atom(ctx, data_atom),
                topic, data
            );
            mailbox_send(target, msg);
            break;
        }
        case MQTT_EVENT_ERROR: {
            ESP_LOGE(TAG, "MQTT_EVENT_ERROR");
            if (UNLIKELY(memory_ensure_free(ctx, 3 + 6) != MEMORY_GC_OK)) {
                mailbox_send(target, MEMORY_ATOM);
                return ESP_OK;
            }
            esp_mqtt_error_codes_t *mqtt_error = event->error_handle;
            term error = create_tuple5(ctx,
                error_type_to_atom(ctx, mqtt_error->error_type),
                connect_return_code_to_atom(ctx, mqtt_error->connect_return_code),
                term_from_int(mqtt_error->esp_tls_last_esp_err),
                term_from_int(mqtt_error->esp_tls_stack_err),
                term_from_int(mqtt_error->esp_tls_cert_verify_flags)
            );
            term msg = port_create_tuple3(ctx,
                context_make_atom(ctx, mqtt_atom),
                ERROR_ATOM,
                error
            );
            mailbox_send(target, msg);
            break;
        }
        case MQTT_EVENT_BEFORE_CONNECT: {
            ESP_LOGI(TAG, "MQTT_EVENT_BEFORE_CONNECT msg_id: %d", event->msg_id);
            break;
        }
        default:
            ESP_LOGW(TAG, "Other event.  event_id: %d", event->event_id);
            break;
    }
    return ESP_OK;
}


static term error_type_to_atom(Context *ctx, esp_mqtt_error_type_t error_type)
{
    switch (error_type) {
        case MQTT_ERROR_TYPE_ESP_TLS:
            return context_make_atom(ctx, esp_tls_atom);
        case MQTT_ERROR_TYPE_CONNECTION_REFUSED:
            return context_make_atom(ctx, connection_refused_atom);
        default:
            return UNDEFINED_ATOM;
    }
}


static term connect_return_code_to_atom(Context *ctx, esp_mqtt_connect_return_code_t connect_return_code)
{
    switch (connect_return_code) {
        case MQTT_CONNECTION_ACCEPTED:
            return context_make_atom(ctx, connection_accepted_atom);
        case MQTT_CONNECTION_REFUSE_PROTOCOL:
            return context_make_atom(ctx, protocol_atom);
        case MQTT_CONNECTION_REFUSE_ID_REJECTED:
            return context_make_atom(ctx, id_rejected_atom);
        case MQTT_CONNECTION_REFUSE_SERVER_UNAVAILABLE:
            return context_make_atom(ctx, server_unavailable_atom);
        case MQTT_CONNECTION_REFUSE_BAD_USERNAME:
            return context_make_atom(ctx, bad_username_atom);
        case MQTT_CONNECTION_REFUSE_NOT_AUTHORIZED:
            return context_make_atom(ctx, not_authorized_atom);
        default:
            return UNDEFINED_ATOM;
    }
}


static term make_atom(GlobalContext *global, const char *string)
{
    int global_atom_index = globalcontext_insert_atom(global, (AtomString) string);
    return term_from_atom_index(global_atom_index);
}


static term create_tuple4(Context *ctx, term a, term b, term c, term d)
{
    term terms[4];
    terms[0] = a;
    terms[1] = b;
    terms[2] = c;
    terms[3] = d;

    return port_create_tuple_n(ctx, 4, terms);
}


static term create_tuple5(Context *ctx, term a, term b, term c, term d, term e)
{
    term terms[4];
    terms[0] = a;
    terms[1] = b;
    terms[2] = c;
    terms[3] = d;
    terms[4] = e;

    return port_create_tuple_n(ctx, 5, terms);
}
