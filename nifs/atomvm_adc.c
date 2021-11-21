//
// Copyright (c) 2020 dushin.net
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

#include <stdlib.h>

#include <esp_adc_cal.h>
#include <esp_log.h>
#include <driver/adc.h>

#include <context.h>
#include <defaultatoms.h>
#include <interop.h>
#include <nifs.h>
#include <term.h>

#include "atomvm_adc.h"

//#define ENABLE_TRACE
#include "trace.h"

#define TAG "atomvm_adc"
#define DEFAULT_SAMPLES 64
#define DEFAULT_VREF    1100

// References
// https://docs.espressif.com/projects/esp-idf/en/v3.3.4/api-reference/peripherals/adc.html
//

static const char *const bit_9_atom           = "\x5"  "bit_9";
static const char *const bit_10_atom          = "\x6"  "bit_10";
static const char *const bit_11_atom          = "\x6"  "bit_11";
static const char *const bit_12_atom          = "\x6"  "bit_12";

static const char *const db_0_atom            = "\x4"  "db_0";
static const char *const db_2_5_atom          = "\x6"  "db_2_5";
static const char *const db_6_atom            = "\x4"  "db_6";
static const char *const db_11_atom           = "\x5"  "db_11";

static const char *const samples_atom         = "\x7"  "samples";
static const char *const raw_atom             = "\x3"  "raw";
static const char *const voltage_atom         = "\x7"  "voltage";
//                                                      123456789ABCDEF01

static adc_bits_width_t get_width(Context *ctx, term width)
{
    if (width == context_make_atom(ctx, bit_9_atom)) {
        return ADC_WIDTH_BIT_9;
    } else if (width == context_make_atom(ctx, bit_10_atom)) {
        return ADC_WIDTH_BIT_10;
    } else if (width == context_make_atom(ctx, bit_11_atom)) {
        return ADC_WIDTH_BIT_11;
    } else if (width == context_make_atom(ctx, bit_12_atom)) {
        return ADC_WIDTH_BIT_12;
    } else {
        return ADC_WIDTH_MAX;
    }
}

static term nif_adc_config_width(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term width = argv[0];
    VALIDATE_VALUE(width, term_is_atom);
    adc_bits_width_t bit_width = get_width(ctx, width);
    if (bit_width == ADC_WIDTH_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    esp_err_t err = adc1_config_width(bit_width);
    if (err != ESP_OK) {
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
    }
    TRACE("Width set to %u\n", bit_width);
    return OK_ATOM;
}

static adc_atten_t get_attenuation(Context *ctx, term attenuation)
{
    if (attenuation == context_make_atom(ctx, db_0_atom)) {
        return ADC_ATTEN_DB_0;
    } else if (attenuation == context_make_atom(ctx, db_2_5_atom)) {
        return ADC_ATTEN_DB_2_5;
    } else if (attenuation == context_make_atom(ctx, db_6_atom)) {
        return ADC_ATTEN_DB_6;
    } else if (attenuation == context_make_atom(ctx, db_11_atom)) {
        return ADC_ATTEN_DB_11;
    } else {
        return ADC_ATTEN_MAX;
    }
}

static adc_channel_t get_channel(avm_int_t pin_val)
{
    switch (pin_val) {
        case 32:
            return ADC1_CHANNEL_4;
        case 33:
            return ADC1_CHANNEL_5;
        case 34:
            return ADC1_CHANNEL_6;
        case 35:
            return ADC1_CHANNEL_7;
        case 36:
            return ADC1_CHANNEL_0;
        case 37:
            return ADC1_CHANNEL_1;
        case 38:
            return ADC1_CHANNEL_2;
        case 39:
            return ADC1_CHANNEL_3;
        default:
            return ADC1_CHANNEL_MAX;
    }
}

static term nif_adc_config_channel_attenuation(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term pin = argv[0];
    VALIDATE_VALUE(pin, term_is_integer);
    adc_channel_t channel = get_channel(term_to_int(pin));
    if ((adc1_channel_t) channel == ADC1_CHANNEL_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term attenuation = argv[1];
    VALIDATE_VALUE(attenuation, term_is_atom);
    adc_atten_t atten = get_attenuation(ctx, attenuation);
    if (atten == ADC_ATTEN_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    esp_err_t err = adc1_config_channel_atten(channel, atten);
    if (err != ESP_OK) {
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
    }
    TRACE("Attenuation on channel %u set to %u\n", channel, atten);
    return OK_ATOM;
}

static void log_char_val_type(esp_adc_cal_value_t val_type)
{
    if (val_type == ESP_ADC_CAL_VAL_EFUSE_TP) {
        TRACE("Characterized using Two Point Value\n");
    } else if (val_type == ESP_ADC_CAL_VAL_EFUSE_VREF) {
        TRACE("Characterized using eFuse Vref\n");
    } else {
        TRACE("Characterized using Default Vref\n");
    }
}

static term nif_adc_take_reading(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term pin = argv[0];
    VALIDATE_VALUE(pin, term_is_integer);
    adc_channel_t channel = get_channel(term_to_int(pin));
    TRACE("take_reading channel: %u\n", channel);
    if ((adc1_channel_t) channel == ADC1_CHANNEL_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term read_options = argv[1];
    VALIDATE_VALUE(read_options, term_is_list);
    term samples = interop_proplist_get_value_default(
        read_options, context_make_atom(ctx, samples_atom), term_from_int(DEFAULT_SAMPLES)
    );
    avm_int_t samples_val = term_to_int(samples);
    TRACE("take_reading samples: %i\n", samples_val);
    term raw = interop_proplist_get_value_default(
        read_options, context_make_atom(ctx, raw_atom), FALSE_ATOM
    );
    term voltage = interop_proplist_get_value_default(
        read_options, context_make_atom(ctx, voltage_atom), FALSE_ATOM
    );

    term width = argv[2];
    VALIDATE_VALUE(width, term_is_atom);
    adc_bits_width_t bit_width = get_width(ctx, width);
    if (bit_width == ADC_WIDTH_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term attenuation = argv[3];
    VALIDATE_VALUE(attenuation, term_is_atom);
    adc_atten_t atten = get_attenuation(ctx, attenuation);
    if (atten == ADC_ATTEN_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    esp_adc_cal_characteristics_t *adc_chars = calloc(1, sizeof(esp_adc_cal_characteristics_t));
    if (IS_NULL_PTR(adc_chars)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    esp_adc_cal_value_t val_type =
        esp_adc_cal_characterize(ADC_UNIT_1, atten, bit_width, DEFAULT_VREF, adc_chars);
    log_char_val_type(val_type);

    uint32_t adc_reading = 0;
    for (avm_int_t i = 0;  i < samples_val;  ++i) {
        adc_reading += adc1_get_raw((adc1_channel_t) channel);
    }
    adc_reading /= samples_val;
    TRACE("take_reading adc_reading: %i\n", adc_reading);

    raw = term_equals(raw, TRUE_ATOM, ctx) ? term_from_int32(adc_reading) : UNDEFINED_ATOM;
    if (term_equals(voltage, TRUE_ATOM, ctx)) {
        voltage = term_from_int32(esp_adc_cal_raw_to_voltage(adc_reading, adc_chars));
    } else {
        voltage = UNDEFINED_ATOM;
    };

    free(adc_chars);

    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    } else {
        term tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(tuple, 0, raw);
        term_put_tuple_element(tuple, 1, voltage);
        return tuple;
    }
}


static const struct Nif adc_config_width_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_config_width
};
static const struct Nif adc_config_channel_attenuation_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_config_channel_attenuation
};
static const struct Nif adc_take_reading_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_adc_take_reading
};


//
// Component Nif Entrypoints
//

void atomvm_adc_init(GlobalContext *global)
{
    //Check TP is burned into eFuse
    if (esp_adc_cal_check_efuse(ESP_ADC_CAL_VAL_EFUSE_TP) == ESP_OK) {
        ESP_LOGI(TAG, "eFuse Two Point: Supported");
    } else {
        ESP_LOGI(TAG, "eFuse Two Point: NOT supported");
    }

    //Check Vref is burned into eFuse
    if (esp_adc_cal_check_efuse(ESP_ADC_CAL_VAL_EFUSE_VREF) == ESP_OK) {
        ESP_LOGI(TAG, "eFuse Vref: Supported");
    } else {
        ESP_LOGI(TAG, "eFuse Vref: NOT supported");
    }
}

const struct Nif *atomvm_adc_get_nif(const char *nifname)
{
    TRACE("Locating nif %s ...", nifname);
    if (strcmp("adc:config_width/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &adc_config_width_nif;
    }
    if (strcmp("adc:config_channel_attenuation/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &adc_config_channel_attenuation_nif;
    }
    if (strcmp("adc:take_reading/4", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &adc_take_reading_nif;
    }
    return NULL;
}
