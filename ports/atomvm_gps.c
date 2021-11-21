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

// #define ENABLE_TRACE
#include <trace.h>

#include "atomvm_gps.h"
#include "nmea_parser.h"


#define TAG "atomvm_gps"

#define YEAR_BASE (2000) //date in GPS starts from 2000
#define NUM_ENTRIES 10

static const char *const stop_atom =                    "\x4" "stop";
static const char *const receiver_atom =                "\x8" "receiver";
static const char *const config_atom =                  "\x6" "config";
static const char *const uart_port_atom =               "\x9" "uart_port";
static const char *const uart_0_atom =                  "\x6" "uart_0";
static const char *const uart_1_atom =                  "\x6" "uart_1";
static const char *const uart_2_atom =                  "\x6" "uart_2";
static const char *const rx_pin_atom =                  "\x6" "rx_pin";
static const char *const baud_rate_atom =               "\x9" "baud_rate";
static const char *const data_bits_atom =               "\x9" "data_bits";
static const char *const data_bits_5_atom =             "\xB" "data_bits_5";
static const char *const data_bits_6_atom =             "\xB" "data_bits_6";
static const char *const data_bits_7_atom =             "\xB" "data_bits_7";
static const char *const data_bits_8_atom =             "\xB" "data_bits_8";
static const char *const stop_bits_atom =               "\x9" "stop_bits";
static const char *const stop_bits_1_atom =             "\xB" "stop_bits_1";
static const char *const stop_bits_1_5_atom =           "\xD" "stop_bits_1_5";
static const char *const stop_bits_2_atom =             "\xB" "stop_bits_2";
static const char *const parity_atom =                  "\x6" "parity";
static const char *const disable_atom =                 "\x7" "disable";
static const char *const even_atom =                    "\x4" "even";
static const char *const odd_atom =                     "\x3" "odd";
static const char *const event_queue_size_atom =       "\x10" "event_queue_size";


static const char *const datetime_atom =                "\x8" "datetime";
static const char *const gps_reading_atom =             "\xB" "gps_reading";
static const char *const latitude_atom =                "\x8" "latitude";
static const char *const longitude_atom =               "\x9" "longitude";
static const char *const altitude_atom =                "\x8" "altitude";
static const char *const speed_atom =                   "\x5" "speed";
static const char *const sats_in_use_atom =             "\xB" "sats_in_use";
static const char *const fix_atom =                     "\x3" "fix";
static const char *const invalid_atom =                 "\x7" "invalid";
static const char *const gps_atom =                     "\x3" "gps";
static const char *const dgps_atom =                    "\x4" "dgps";
static const char *const fix_mode_atom =                "\x8" "fix_mode";
static const char *const mode_2d_atom =                 "\x7" "mode_2d";
static const char *const mode_3d_atom =                 "\x7" "mode_3d";
static const char *const valid_atom =                   "\x5" "valid";
static const char *const sats_in_view_atom =            "\xC" "sats_in_view";
static const char *const num_atom =                     "\x3" "num";
static const char *const elevation_atom =               "\x9" "elevation";
static const char *const azimuth_atom =                 "\x7" "azimuth";
static const char *const snr_atom =                     "\x3" "snr";
//                                                            0123456789ABCDEF0123456789ABCDEF
//                                                            0               1


struct platform_data {
    nmea_parser_handle_t parser;
    term receiver;
};

static term float_to_term(Context *ctx, float f)
{
    int integral = (int) f;
    int fractional = (int) abs((f - integral) * 10000);
    return port_create_tuple2(ctx, term_from_int(integral), term_from_int(fractional));
}

static term fix_to_term(Context *ctx, gps_fix_t fix)
{
    switch(fix) {
        case GPS_FIX_INVALID:
            return context_make_atom(ctx, invalid_atom);
        case GPS_FIX_GPS:
            return context_make_atom(ctx, gps_atom);
        case GPS_FIX_DGPS:
            return context_make_atom(ctx, dgps_atom);
    }
    // should never happen
    return term_invalid_term();
}

static term fix_mode_to_term(Context *ctx, gps_fix_mode_t fix_mode)
{
    switch(fix_mode) {
        case GPS_MODE_INVALID:
            return context_make_atom(ctx, invalid_atom);
        case GPS_MODE_2D:
            return context_make_atom(ctx, mode_2d_atom);
        case GPS_MODE_3D:
            return context_make_atom(ctx, mode_3d_atom);
    }
    // should never happen
    return term_invalid_term();
}

static term satellite_to_term(Context *ctx, gps_satellite_t *sat)
{
    term map = term_alloc_map(ctx, 4);

    term_set_map_assoc(map, 0, context_make_atom(ctx, num_atom), term_from_int(sat->num));
    term_set_map_assoc(map, 1, context_make_atom(ctx, elevation_atom), term_from_int(sat->elevation));
    term_set_map_assoc(map, 2, context_make_atom(ctx, azimuth_atom), term_from_int(sat->azimuth));
    term_set_map_assoc(map, 3, context_make_atom(ctx, snr_atom), term_from_int(sat->snr));

    return map;
}

static term sats_in_use_to_term(Context *ctx, uint8_t sats_in_use, uint8_t *sat_ids)
{
    term ret = term_nil();

    for (int i = sats_in_use - 1;  i >= 0;  --i) {
        ret = term_list_prepend(term_from_int(sat_ids[i]), ret, ctx);
    }
    return ret;
}

static term sats_in_view_to_term(Context *ctx, uint8_t sats_in_view, gps_satellite_t *sats)
{
    term ret = term_nil();

    for (int i = sats_in_view - 1;  i >= 0;  --i) {
        ret = term_list_prepend(satellite_to_term(ctx, sats + i), ret, ctx);
    }
    return ret;
}

static term bool_to_term(bool b)
{
    return b ? TRUE_ATOM : FALSE_ATOM;
}

static term gps_to_term(Context *ctx, gps_t *gps)
{
    term date = port_create_tuple3(ctx,
        term_from_int(gps->date.year + YEAR_BASE),
        term_from_int(gps->date.month),
        term_from_int(gps->date.day)
    );
    term time = port_create_tuple3(ctx,
        term_from_int(gps->tim.hour),
        term_from_int(gps->tim.minute),
        term_from_int(gps->tim.second)
    );
    term datetime = port_create_tuple2(ctx, date, time);

    term map = term_alloc_map(ctx, NUM_ENTRIES);
    term_set_map_assoc(map, 0, context_make_atom(ctx, datetime_atom), datetime);
    term_set_map_assoc(map, 1, context_make_atom(ctx, latitude_atom), float_to_term(ctx, gps->latitude));
    term_set_map_assoc(map, 2, context_make_atom(ctx, longitude_atom), float_to_term(ctx, gps->longitude));
    term_set_map_assoc(map, 3, context_make_atom(ctx, altitude_atom), float_to_term(ctx, gps->altitude));
    term_set_map_assoc(map, 4, context_make_atom(ctx, speed_atom), float_to_term(ctx, gps->speed));
    term_set_map_assoc(map, 5, context_make_atom(ctx, sats_in_use_atom), sats_in_use_to_term(ctx, gps->sats_in_use, gps->sats_id_in_use));
    term_set_map_assoc(map, 6, context_make_atom(ctx, fix_atom), fix_to_term(ctx, gps->fix));
    term_set_map_assoc(map, 7, context_make_atom(ctx, fix_mode_atom), fix_mode_to_term(ctx, gps->fix_mode));
    term_set_map_assoc(map, 8, context_make_atom(ctx, valid_atom), bool_to_term(gps->valid));
    term_set_map_assoc(map, 9, context_make_atom(ctx, sats_in_view_atom), sats_in_view_to_term(ctx, gps->sats_in_view, gps->sats_desc_in_view));

    return map;
}

static void gps_event_handler(void *event_handler_arg, esp_event_base_t event_base, int32_t event_id, void *event_data)
{
    Context *ctx = (Context *) event_handler_arg;
    struct platform_data *plfdat = (struct platform_data *) ctx->platform_data;

    int pid = term_to_local_process_id(plfdat->receiver);
    Context *target = globalcontext_get_process(ctx->global, pid);

    gps_t *gps = NULL;
    switch (event_id) {
    case GPS_UPDATE:
        gps = (gps_t *)event_data;

        // date_time: pair of 3-ary tuples = 4*2 + 3 = 11
        // latitude: pair = 3
        // longitude: pair = 3
        // altitude: pair = 3
        // speed: pair = 3
        // sats_in_use: list[int (max=16)] = 12*2 = 24
        // sats_in_view: list[4-tuple of int (max=16)] = 16*10 = 160
        // fix: atom = 0
        // fix_mode: atom = 0
        // valid: atom = 0
        // {gps_reading, GPSReading} = 3
        // total: 210
        // TODO: make a more accurate calculation of size based on actual sats_in_view
        if (UNLIKELY(memory_ensure_free(ctx, term_map_size_in_terms(NUM_ENTRIES) + 210) != MEMORY_GC_OK)) {
            mailbox_send(target, MEMORY_ATOM);
            return NULL;
        }

        term gps_reading = gps_to_term(ctx, gps);
        term msg = port_create_tuple2(ctx,
            context_make_atom(ctx, gps_reading_atom),
            gps_reading
        );

        mailbox_send(target, msg);

        break;
    case GPS_UNKNOWN:
        ESP_LOGW(TAG, "Unknown statement:%s", (char *)event_data);
        break;
    default:
        break;
    }
}


static void do_stop(Context *ctx)
{
    struct platform_data *plfdat = (struct platform_data *) ctx->platform_data;
    nmea_parser_handle_t parser = plfdat->parser;

    TRACE(TAG ": do_stop\n");
    nmea_parser_deinit(parser);
    scheduler_terminate(ctx);
    free(plfdat);
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
            ret = OK_ATOM;
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


static term make_atom(GlobalContext *global, const char *string)
{
    int global_atom_index = globalcontext_insert_atom(global, (AtomString) string);
    return term_from_atom_index(global_atom_index);
}


static uart_port_t get_uart_port(Context *ctx, term config)
{
    term uart_port = interop_map_get_value_default(
        ctx, config, context_make_atom(ctx, uart_port_atom), context_make_atom(ctx, uart_1_atom)
    );
    if (uart_port == context_make_atom(ctx, uart_0_atom)) {
        return UART_NUM_0;
    }
    if (uart_port == context_make_atom(ctx, uart_1_atom)) {
        return UART_NUM_1;
    }
    if (uart_port == context_make_atom(ctx, uart_2_atom)) {
        return UART_NUM_2;
    }
    ESP_LOGE(TAG, "Invalid uart_port.");
    return UART_NUM_MAX;
}


static int get_integer_value(Context *ctx, term config, term key, int default_value)
{
    term value = interop_map_get_value_default(
        ctx, config, key, term_invalid_term()
    );
    if (term_is_invalid_term(value)) {
        return default_value;
    } else if (term_is_integer(value)) {
        return term_to_int(value);
    }
    ESP_LOGE(TAG, "Invalid integer value.");
    return -1;
}


static int get_rx_pin(Context *ctx, term config)
{
    term rx_pin = interop_map_get_value_default(
        ctx, config, context_make_atom(ctx, rx_pin_atom), term_invalid_term()
    );
    if (term_is_invalid_term(rx_pin)) {
        return UART_PIN_NO_CHANGE;
    } else if (term_is_integer(rx_pin)) {
        return term_to_int(rx_pin);
    }
    ESP_LOGE(TAG, "Invalid rx_pin.");
    return UART_PIN_NO_CHANGE;
}


static int get_baud_rate(Context *ctx, term config)
{
    return get_integer_value(ctx, config, context_make_atom(ctx, baud_rate_atom), 9600);
}


static uart_word_length_t get_data_bits(Context *ctx, term config)
{
    term data_bits = interop_map_get_value_default(
        ctx, config, context_make_atom(ctx, data_bits_atom), context_make_atom(ctx, data_bits_8_atom)
    );
    if (data_bits == context_make_atom(ctx, data_bits_5_atom)) {
        return UART_DATA_5_BITS;
    }
    if (data_bits == context_make_atom(ctx, data_bits_6_atom)) {
        return UART_DATA_6_BITS;
    }
    if (data_bits == context_make_atom(ctx, data_bits_7_atom)) {
        return UART_DATA_7_BITS;
    }
    if (data_bits == context_make_atom(ctx, data_bits_8_atom)) {
        return UART_DATA_8_BITS;
    }
    ESP_LOGE(TAG, "Invalid data_bits.");
    return UART_DATA_BITS_MAX;
}


static uart_stop_bits_t get_stop_bits(Context *ctx, term config)
{
    term stop_bits = interop_map_get_value_default(
        ctx, config, context_make_atom(ctx, stop_bits_atom), context_make_atom(ctx, stop_bits_1_atom)
    );
    if (stop_bits == context_make_atom(ctx, stop_bits_1_atom)) {
        return UART_STOP_BITS_1;
    }
    if (stop_bits == context_make_atom(ctx, stop_bits_1_5_atom)) {
        return UART_STOP_BITS_1_5;
    }
    if (stop_bits == context_make_atom(ctx, stop_bits_2_atom)) {
        return UART_STOP_BITS_2;
    }
    ESP_LOGE(TAG, "Invalid stop_bits.");
    return UART_STOP_BITS_MAX;
}


static uart_parity_t get_parity(Context *ctx, term config)
{
    term parity = interop_map_get_value_default(
        ctx, config, context_make_atom(ctx, parity_atom), context_make_atom(ctx, disable_atom)
    );
    if (parity == context_make_atom(ctx, disable_atom)) {
        return UART_PARITY_DISABLE;
    }
    if (parity == context_make_atom(ctx, even_atom)) {
        return UART_PARITY_EVEN;
    }
    if (parity == context_make_atom(ctx, odd_atom)) {
        return UART_PARITY_ODD;
    }
    ESP_LOGE(TAG, "Invalid parity.");
    return UART_PARITY_DISABLE;
}


static int get_event_queue_size(Context *ctx, term config)
{
    return get_integer_value(ctx, config, context_make_atom(ctx, event_queue_size_atom), 16);
}


//
// Entrypoints
//

void atomvm_gps_init(GlobalContext *global)
{
    esp_log_level_set(TAG, ESP_LOG_VERBOSE);
}

Context *atomvm_gps_create_port(GlobalContext *global, term opts)
{
    term receiver = interop_proplist_get_value(opts, make_atom(global, receiver_atom));
    term config = interop_proplist_get_value(opts, make_atom(global, config_atom));


    Context *ctx = context_new(global);
    ctx->native_handler = consume_mailbox;

    nmea_parser_config_t parser_config = // NMEA_PARSER_CONFIG_DEFAULT();
    {
        .uart = {
            .uart_port = get_uart_port(ctx, config),
            .rx_pin = get_rx_pin(ctx, config),
            .baud_rate = get_baud_rate(ctx, config),
            .data_bits = get_data_bits(ctx, config),
            .parity = get_parity(ctx, config),
            .stop_bits = get_stop_bits(ctx, config),
            .event_queue_size = get_event_queue_size(ctx, config)
        }
    };

    nmea_parser_handle_t parser = nmea_parser_init(&parser_config);
    if (UNLIKELY(IS_NULL_PTR(parser))) {
        context_destroy(ctx);
        ESP_LOGE(TAG, "Error: Unable to initialize nmea parser.\n");
        return NULL;
    }

    esp_err_t err = nmea_parser_add_handler(parser, gps_event_handler, ctx);
    if (err != ESP_OK) {
        context_destroy(ctx);
        nmea_parser_deinit(parser);
        ESP_LOGE(TAG, "Error: Unable to add nmea handler.  Error: %i.\n", err);
        return NULL;
    }

    struct platform_data *plfdat = malloc(sizeof(struct platform_data));
    plfdat->parser = parser;
    plfdat->receiver = receiver;
    ctx->platform_data = plfdat;

    ESP_LOGI(TAG, "atomvm_gps started.");
    return ctx;
}
