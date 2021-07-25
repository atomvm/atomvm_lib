//
// Copyright (c) 2021 dushin.net
// Copyright (c) y.yanbe@gmail.com
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

#include <string.h>
#include <stdlib.h>

#include <context.h>
#include <defaultatoms.h>
#include <interop.h>
#include <nifs.h>
#include <term.h>
#include <driver/gpio.h>
#include <driver/i2c.h>
#include <esp_err.h>
#include <esp_log.h>

#include "atomvm_ssd1306.h"
#include "font8x8_basic.h"

// #define ENABLE_TRACE
#include <trace.h>

// MODIFIED FROM https://github.com/yanbe/ssd1306-esp-idf-i2c

// SLA (0x3C) + WRITE_MODE (0x00) =  0x78 (0b01111000)
#define OLED_I2C_ADDRESS   0x3C

// Control byte
#define OLED_CONTROL_BYTE_CMD_SINGLE    0x80
#define OLED_CONTROL_BYTE_CMD_STREAM    0x00
#define OLED_CONTROL_BYTE_DATA_STREAM   0x40

// Fundamental commands (pg.28)
#define OLED_CMD_SET_CONTRAST           0x81    // follow with 0x7F
#define OLED_CMD_DISPLAY_RAM            0xA4
#define OLED_CMD_DISPLAY_ALLON          0xA5
#define OLED_CMD_DISPLAY_NORMAL         0xA6
#define OLED_CMD_DISPLAY_INVERTED       0xA7
#define OLED_CMD_DISPLAY_OFF            0xAE
#define OLED_CMD_DISPLAY_ON             0xAF

// Addressing Command Table (pg.30)
#define OLED_CMD_SET_MEMORY_ADDR_MODE   0x20    // follow with 0x00 = HORZ mode = Behave like a KS108 graphic LCD
#define OLED_CMD_SET_COLUMN_RANGE       0x21    // can be used only in HORZ/VERT mode - follow with 0x00 and 0x7F = COL127
#define OLED_CMD_SET_PAGE_RANGE         0x22    // can be used only in HORZ/VERT mode - follow with 0x00 and 0x07 = PAGE7

// Hardware Config (pg.31)
#define OLED_CMD_SET_DISPLAY_START_LINE 0x40
#define OLED_CMD_SET_SEGMENT_REMAP      0xA1
#define OLED_CMD_SET_MUX_RATIO          0xA8    // follow with 0x3F = 64 MUX
#define OLED_CMD_SET_COM_SCAN_MODE      0xC8
#define OLED_CMD_SET_DISPLAY_OFFSET     0xD3    // follow with 0x00
#define OLED_CMD_SET_COM_PIN_MAP        0xDA    // follow with 0x12
#define OLED_CMD_NOP                    0xE3    // NOP

// Timing and Driving Scheme (pg.32)
#define OLED_CMD_SET_DISPLAY_CLK_DIV    0xD5    // follow with 0x80
#define OLED_CMD_SET_PRECHARGE          0xD9    // follow with 0xF1
#define OLED_CMD_SET_VCOMH_DESELCT      0xDB    // follow with 0x30

// Charge Pump (pg.62)
#define OLED_CMD_SET_CHARGE_PUMP        0x8D    // follow with 0x14

#define MASTER_COMMAND_TIMEOUT_MS       (100/portTICK_PERIOD_MS)

#define TAG "atomvm_ssd1306"

static const char *const sda_pin_atom =     "\x7" "sda_pin";
static const char *const scl_pin_atom =     "\x7" "scl_pin";
static const char *const freq_hz_atom =     "\x7" "freq_hz";
static const char *const i2c_num_atom =     "\x7" "i2c_num";
static const char *const i2c_num_0_atom =   "\x9" "i2c_num_0";
static const char *const i2c_num_1_atom =   "\x9" "i2c_num_1";

        // RAISE_ERROR(ERROR_ATOM);
#define I2C_MASTER_WRITE_BYTE(CMD, BYTE, ACK) {\
    esp_err_t err = i2c_master_write_byte(CMD, BYTE, ACK); \
    if (err != ESP_OK) { \
        i2c_cmd_link_delete(CMD); \
    } \
}

#define I2C_MASTER_WRITE(CMD, DATA, LEN, ACK) {\
    esp_err_t err = i2c_master_write(CMD, DATA, LEN, ACK); \
    if (err != ESP_OK) { \
        i2c_cmd_link_delete(CMD); \
    } \
}

static i2c_port_t get_i2c_port_num(Context *ctx, term t)
{
    if (t == context_make_atom(ctx, i2c_num_0_atom)) {
        return I2C_NUM_0;
    } else if (t == context_make_atom(ctx, i2c_num_1_atom)) {
        return I2C_NUM_1;
    } else {
        return I2C_NUM_MAX;
    }
}

static esp_err_t init_ssd1306_command(i2c_port_t i2c_num)
{
    i2c_cmd_handle_t cmd = i2c_cmd_link_create();
    i2c_master_start(cmd);
        I2C_MASTER_WRITE_BYTE(cmd, (OLED_I2C_ADDRESS << 1) | I2C_MASTER_WRITE, true);
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CONTROL_BYTE_CMD_STREAM, true);
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_CHARGE_PUMP, true);
        I2C_MASTER_WRITE_BYTE(cmd, 0x14, true);
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_SEGMENT_REMAP, true); // reverse left-right mapping
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_COM_SCAN_MODE, true); // reverse up-bottom mapping
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_DISPLAY_ON, true);
    i2c_master_stop(cmd);
    esp_err_t err = i2c_master_cmd_begin(i2c_num, cmd, MASTER_COMMAND_TIMEOUT_MS);
    i2c_cmd_link_delete(cmd);
    return err;
}

static term nif_ssd1306_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term config = argv[0];
    VALIDATE_VALUE(config, term_is_map);

    term sda_pin_term = interop_map_get_value(ctx, config, context_make_atom(ctx, sda_pin_atom));
    VALIDATE_VALUE(sda_pin_term, term_is_integer);
    term scl_pin_term = interop_map_get_value(ctx, config, context_make_atom(ctx, scl_pin_atom));
    VALIDATE_VALUE(scl_pin_term, term_is_integer);
    term freq_hz_term = interop_map_get_value(ctx, config, context_make_atom(ctx, freq_hz_atom));
    VALIDATE_VALUE(freq_hz_term, term_is_integer);
    term i2c_num_term = interop_map_get_value(ctx, config, context_make_atom(ctx, i2c_num_atom));
    VALIDATE_VALUE(i2c_num_term, term_is_atom);

    i2c_port_t i2c_num = get_i2c_port_num(ctx, i2c_num_term);
    if (i2c_num == I2C_NUM_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    //
    // Configure I2C
    //
    i2c_config_t i2c_config = {
        .mode = I2C_MODE_MASTER,
        .sda_io_num = term_to_int(sda_pin_term),
        .scl_io_num = term_to_int(scl_pin_term),
        .sda_pullup_en = GPIO_PULLUP_ENABLE,
        .scl_pullup_en = GPIO_PULLUP_ENABLE,
        .master.clk_speed = term_to_int(freq_hz_term)
    };
    esp_err_t err;
    err = i2c_param_config(i2c_num, &i2c_config);
    if (err != ESP_OK) {
        ESP_LOGE(TAG, "I2C configuration failed.  Error: %i", err);
        RAISE_ERROR(ERROR_ATOM);
    }
    //
    // Install the I2C driver
    //
    err = i2c_driver_install(i2c_num, I2C_MODE_MASTER, 0, 0, 0);
    if (err != ESP_OK) {
        ESP_LOGE(TAG, "Failed to install I2C driver.  Error: %i", err);
        RAISE_ERROR(ERROR_ATOM);
    }
    //
    // Initialize the SSD1306 device
    //
    err = init_ssd1306_command(i2c_num);
    if (err != ESP_OK) {
        i2c_driver_delete(i2c_num);
        ESP_LOGE(TAG, "SSD1306 initialization failed.  Error: %i", err);
        RAISE_ERROR(ERROR_ATOM);
    }

    ESP_LOGI(TAG, "SSD1306 initialized.");
    return OK_ATOM;
}

static esp_err_t write_page_command(i2c_port_t i2c_num, uint8_t *page, uint8_t pagenum)
{
    i2c_cmd_handle_t cmd = i2c_cmd_link_create();
    i2c_master_start(cmd);
        I2C_MASTER_WRITE_BYTE(cmd, (OLED_I2C_ADDRESS << 1) | I2C_MASTER_WRITE, true);
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CONTROL_BYTE_CMD_SINGLE, true);
        I2C_MASTER_WRITE_BYTE(cmd, 0xB0 | pagenum, true);
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CONTROL_BYTE_DATA_STREAM, true);
        I2C_MASTER_WRITE(cmd, page, 128, true);
    i2c_master_stop(cmd);
    esp_err_t err = i2c_master_cmd_begin(i2c_num, cmd, MASTER_COMMAND_TIMEOUT_MS);
    i2c_cmd_link_delete(cmd);
    return err;
}

static term nif_ssd1306_clear(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term i2c_num_term = argv[0];
    VALIDATE_VALUE(i2c_num_term, term_is_atom);

    i2c_port_t i2c_num = get_i2c_port_num(ctx, i2c_num_term);
    if (i2c_num == I2C_NUM_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint8_t zero[128];
    memset(zero, 0, sizeof zero);
    for (uint8_t i = 0; i < 8; i++) {
        esp_err_t err = write_page_command(i2c_num, zero, i);
        if (err != ESP_OK) {
            ESP_LOGW(TAG, "Failed to clear page %i.  Error: %i", i, err);
        }
    }

    return OK_ATOM;
}

static esp_err_t set_contrast_command(i2c_port_t i2c_num, uint8_t contrast)
{
    i2c_cmd_handle_t cmd = i2c_cmd_link_create();
    i2c_master_start(cmd);
        I2C_MASTER_WRITE_BYTE(cmd, (OLED_I2C_ADDRESS << 1) | I2C_MASTER_WRITE, true);
		I2C_MASTER_WRITE_BYTE(cmd, OLED_CONTROL_BYTE_CMD_STREAM, true);
		I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_CONTRAST, true);
		I2C_MASTER_WRITE_BYTE(cmd, contrast, true);
    i2c_master_stop(cmd);
    esp_err_t err = i2c_master_cmd_begin(i2c_num, cmd, MASTER_COMMAND_TIMEOUT_MS);
    i2c_cmd_link_delete(cmd);
    return err;
}

static term nif_ssd1306_set_contrast(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term i2c_num_term = argv[0];
    VALIDATE_VALUE(i2c_num_term, term_is_atom);
    term contrast_term = argv[1];
    VALIDATE_VALUE(contrast_term, term_is_integer);

    i2c_port_t i2c_num = get_i2c_port_num(ctx, i2c_num_term);
    if (i2c_num == I2C_NUM_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint8_t contrast = term_to_int(contrast_term) % 256;
    esp_err_t err = set_contrast_command(i2c_num, contrast);
    if (err != ESP_OK) {
        return ERROR_ATOM;
    } else {
        return OK_ATOM;
    }
}

static esp_err_t reset_display_command(i2c_port_t i2c_num)
{
    i2c_cmd_handle_t cmd = i2c_cmd_link_create();
    i2c_master_start(cmd);
        I2C_MASTER_WRITE_BYTE(cmd, (OLED_I2C_ADDRESS << 1) | I2C_MASTER_WRITE, true);
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CONTROL_BYTE_CMD_STREAM, true);
        I2C_MASTER_WRITE_BYTE(cmd, 0x00, true);             // reset column
        I2C_MASTER_WRITE_BYTE(cmd, 0x10, true);
        I2C_MASTER_WRITE_BYTE(cmd, 0xB0, true);             // reset page
    i2c_master_stop(cmd);
    esp_err_t err = i2c_master_cmd_begin(i2c_num, cmd, MASTER_COMMAND_TIMEOUT_MS);
    i2c_cmd_link_delete(cmd);
    return err;
}

static esp_err_t write_text_command(i2c_port_t i2c_num, const char *text, uint8_t text_len)
{
    uint8_t cur_page = 0;
    for (uint8_t i = 0; i < text_len; i++) {
        if (text[i] == '\n') {
            i2c_cmd_handle_t cmd = i2c_cmd_link_create();
            i2c_master_start(cmd);
                I2C_MASTER_WRITE_BYTE(cmd, (OLED_I2C_ADDRESS << 1) | I2C_MASTER_WRITE, true);
                I2C_MASTER_WRITE_BYTE(cmd, OLED_CONTROL_BYTE_CMD_STREAM, true);
                I2C_MASTER_WRITE_BYTE(cmd, 0x00, true);                 // reset column
                I2C_MASTER_WRITE_BYTE(cmd, 0x10, true);
                I2C_MASTER_WRITE_BYTE(cmd, 0xB0 | ++cur_page, true);    // increment page
            i2c_master_stop(cmd);
            esp_err_t err = i2c_master_cmd_begin(i2c_num, cmd, MASTER_COMMAND_TIMEOUT_MS);
            i2c_cmd_link_delete(cmd);
            if (err != ESP_OK) {
                return err;
            }
        } else {
            i2c_cmd_handle_t cmd = i2c_cmd_link_create();
            i2c_master_start(cmd);
                I2C_MASTER_WRITE_BYTE(cmd, (OLED_I2C_ADDRESS << 1) | I2C_MASTER_WRITE, true);
                I2C_MASTER_WRITE_BYTE(cmd, OLED_CONTROL_BYTE_DATA_STREAM, true);
                I2C_MASTER_WRITE(cmd, font8x8_basic_tr[(uint8_t)text[i]], 8, true);
            i2c_master_stop(cmd);
            esp_err_t err = i2c_master_cmd_begin(i2c_num, cmd, MASTER_COMMAND_TIMEOUT_MS);
            i2c_cmd_link_delete(cmd);
            if (err != ESP_OK) {
                return err;
            }
        }
    }
    return ESP_OK;
}

static term nif_ssd1306_set_text(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term i2c_num_term = argv[0];
    VALIDATE_VALUE(i2c_num_term, term_is_atom);
    term text_term = argv[1];
    VALIDATE_VALUE(text_term, term_is_binary);

    i2c_port_t i2c_num = get_i2c_port_num(ctx, i2c_num_term);
    if (i2c_num == I2C_NUM_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const char *text = term_binary_data(text_term);
    uint8_t text_len = term_binary_size(text_term);

    if (reset_display_command(i2c_num) != ESP_OK) {
        RAISE_ERROR(ERROR_ATOM);
    }

    esp_err_t err = write_text_command(i2c_num, text, text_len);
    if (err != ESP_OK) {
        RAISE_ERROR(ERROR_ATOM);
    }

    return OK_ATOM;
}

static inline void set_page_pixel(uint8_t *page, unsigned x, unsigned y)
{
    page[x] ^= (1 << y);
}

static inline bool get_bitmap_value(const uint8_t *bitmap, size_t bit)
{
    return (bitmap[bit / 8] & (1 << (7 - (bit % 8)))) ? true : false;
}

static void write_bitmap_page(i2c_port_t i2c_num, size_t *bit, uint8_t pagenum, const uint8_t *bitmap, size_t width, size_t height)
{
    uint8_t page[128];
    memset(page, 0x00, sizeof page);

    size_t offset = pagenum * 8;
    for (size_t y = 0;  y < 8;  ++y) {
        unsigned yoff = offset + y;
        if (yoff < height) {
            for (size_t x = 0; x < width;  ++x) {
                if (get_bitmap_value(bitmap, *bit)) {
                    // printf("x");
                    set_page_pixel(page, x, y);
                } else {
                    // printf(" ");
                }
                *bit = *bit + 1;
            }
        }
        // printf("\n");
    }
    esp_err_t err = write_page_command(i2c_num, page, pagenum);
    if (err != ESP_OK) {
        ESP_LOGW(TAG, "Failed to write qrcode page %i.  Error: %i", pagenum, err);
    }
}

static term nif_ssd1306_set_bitmap(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term i2c_num_term = argv[0];
    VALIDATE_VALUE(i2c_num_term, term_is_atom);
    term bitmap_term = argv[1];
    VALIDATE_VALUE(bitmap_term, term_is_binary);
    term width_term = argv[2];
    VALIDATE_VALUE(width_term, term_is_integer);
    term height_term = argv[3];
    VALIDATE_VALUE(height_term, term_is_integer);

    i2c_port_t i2c_num = get_i2c_port_num(ctx, i2c_num_term);
    if (i2c_num == I2C_NUM_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const uint8_t *bitmap = (const uint8_t *) term_binary_data(bitmap_term);
    avm_int_t width = term_to_int(width_term);
    avm_int_t height = term_to_int(height_term);

    if (reset_display_command(i2c_num) != ESP_OK) {
        RAISE_ERROR(ERROR_ATOM);
    }

    size_t bit = 0;
    for (uint8_t pagenum = 0; pagenum < 8;  ++pagenum) {
        write_bitmap_page(i2c_num, &bit, pagenum, bitmap, width, height);
    }
    return OK_ATOM;
}

#include "qrcodegen.h"

static void write_qrcode_page(i2c_port_t i2c_num, uint8_t pagenum, const uint8_t *qrcode, unsigned size)
{
    uint8_t page[128];
    memset(page, 0x00, sizeof page);

    unsigned offset = pagenum * 8;
    for (unsigned y = 0;  y < 8;  ++y) {
        unsigned yoff = offset + y;
        if (yoff < size) {
            for (unsigned x = 0; x < size;  ++x) {
                bool v = qrcodegen_getModule(qrcode, x, yoff);
                // printf(v ? "x" : " ");
                if (v) {
                    set_page_pixel(page, x, y);
                }
            }
        }
        // printf("\n");
    }
    esp_err_t err = write_page_command(i2c_num, page, pagenum);
    if (err != ESP_OK) {
        ESP_LOGW(TAG, "Failed to write qrcode page %i.  Error: %i", pagenum, err);
    }
}

static term nif_ssd1306_set_qrcode(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term i2c_num_term = argv[0];
    VALIDATE_VALUE(i2c_num_term, term_is_atom);
    term qrcode_term = argv[1];
    VALIDATE_VALUE(qrcode_term, term_is_binary);

    i2c_port_t i2c_num = get_i2c_port_num(ctx, i2c_num_term);
    if (i2c_num == I2C_NUM_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const uint8_t *qrcode = (const uint8_t *) term_binary_data(qrcode_term);
    unsigned size = (unsigned) qrcodegen_getSize(qrcode);

    if (reset_display_command(i2c_num) != ESP_OK) {
        RAISE_ERROR(ERROR_ATOM);
    }

    for (uint8_t pagenum = 0; pagenum < 8;  ++pagenum) {
        write_qrcode_page(i2c_num, pagenum, qrcode, size);
    }
    return OK_ATOM;
}

//
// Nif integration
//

static const struct Nif ssd1306_init_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssd1306_init
};
static const struct Nif ssd1306_clear_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssd1306_clear
};
static const struct Nif ssd1306_set_contrast_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssd1306_set_contrast
};
static const struct Nif ssd1306_set_text_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssd1306_set_text
};
static const struct Nif ssd1306_set_qrcode_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssd1306_set_qrcode
};
static const struct Nif ssd1306_set_bitmap_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssd1306_set_bitmap
};


void atomvm_ssd1306_init(GlobalContext *global)
{
    // no-op
}
const struct Nif *atomvm_ssd1306_get_nif(const char *nifname)
{
    TRACE("Locating nif %s ...", nifname);
    if (strcmp("ssd1306:nif_init/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ssd1306_init_nif;
    }
    if (strcmp("ssd1306:nif_clear/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ssd1306_clear_nif;
    }
    if (strcmp("ssd1306:nif_set_contrast/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ssd1306_set_contrast_nif;
    }
    if (strcmp("ssd1306:nif_set_text/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ssd1306_set_text_nif;
    }
    if (strcmp("ssd1306:nif_set_bitmap/4", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ssd1306_set_bitmap_nif;
    }
    if (strcmp("ssd1306:nif_set_qrcode/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ssd1306_set_qrcode_nif;
    }
    return NULL;
}
