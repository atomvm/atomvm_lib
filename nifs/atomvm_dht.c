//
// Copyright (c) dushin.net
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

#include <esp_log.h>
#include <driver/gpio.h>
#include <esp_timer.h>
#include <rom/ets_sys.h>

#include <context.h>
#include <defaultatoms.h>
#include <interop.h>
#include <nifs.h>
#include <port.h>
#include <term.h>

#include "atomvm_dht.h"

//#define ENABLE_TRACE
#include "trace.h"

#define TAG "atomvm_dht"

// References
// https://www.electronicwings.com/sensors-modules/dht11
// https://components101.com/sites/default/files/component_datasheet/DHT11-Temperature-Sensor.pdf
// https://cdn-shop.adafruit.com/datasheets/Digital+humidity+and+temperature+sensor+AM2302.pdf

//
// Handshake Send:
//
// ---+                      +---  1 (high)
//     \                    /      |
//      \                  /       |
//       \                /        |
//        +--------------+         0 (low)
//        |              |
//        |<--- 18ms --->|
//
//
//
// Handshake Receive:
//
//
//                            |<---- 80us ---->|
//                            |                |
// ---+                       +----------------+            1 (high)
//     \                     /                  \           |
//      \                   /                    \          |
//       \                 /                      \         |
//        +---------------+                        +------  0 (low)
//        |               |
//        | <--- 80us --->|
//
//
// for each bit (of 40):
//
// Data Receive 0:
//
//                            |<- 28us ->|
//                            |          |
// ---+                       +----------+            1 (high)
//     \                     /            \           |
//      \                   /              \          |
//       \                 /                \         |
//        +---------------+                  +------  0 (low)
//        |               |
//        | <--- 54us --->|
//
//
// Data Receive 1:
//
//                            |<---- 70us ---->|
//                            |                |
// ---+                       +----------------+            1 (high)
//     \                     /                  \           |
//      \                   /                    \          |
//       \                 /                      \         |
//        +---------------+                        +------  0 (low)
//        |               |
//        | <--- 54us --->|
//

#define HANDSHAKE_SEND_LOW_US (18000)
#define HANDSHAKE_RECV_LOW_US (80)
#define HANDSHAKE_RECV_HIGH_US (80)
#define DATA_RECV_LOW_US (54)
#define DATA_RECV_HIGH_ONE_US (70)
#define DATA_RECV_HIGH_ZERO_US (28)

#define MAX_WAIT_US (1000)
#define TIMEOUT (-1)
#define PROTOCOL_ERROR (-1)


static const char *const dht_bad_read         = "\x8"  "bad_read";
//                                                      123456789ABCDEF01


static inline void init_hanshake(avm_int_t pin)
{
    gpio_set_direction(pin, GPIO_MODE_OUTPUT);
    gpio_set_level(pin, 0);
    ets_delay_us(HANDSHAKE_SEND_LOW_US);
    gpio_set_level(pin, 1);
    gpio_set_direction(pin, GPIO_MODE_INPUT);
}

static inline int wait_while(avm_int_t pin, unsigned value, int max_wait)
{
    register int i = 0;
    for (;  gpio_get_level(pin) == value;  ++i) {
        if (max_wait < i) {
            return TIMEOUT;
        }
        ets_delay_us(1);
    }
    return i;
}

static inline int handshake(avm_int_t pin)
{
    init_hanshake(pin);

    if (wait_while(pin, 1, MAX_WAIT_US) == TIMEOUT) {
        TRACE("Timed out waiting to initialze handshake\n");
        return PROTOCOL_ERROR;
    }
    if (wait_while(pin, 0, HANDSHAKE_RECV_LOW_US) == TIMEOUT) {
        TRACE("Timed out waiting to recieve handshake low signal\n");
        return PROTOCOL_ERROR;
    }
    if (wait_while(pin, 1, HANDSHAKE_RECV_HIGH_US) == TIMEOUT) {
        TRACE("Timed out waiting to recieve handshake high signal\n");
        return PROTOCOL_ERROR;
    }

    return 0;
}


static int read_into(avm_int_t pin, uint8_t *buf)
{
    if (handshake(pin) == PROTOCOL_ERROR) {
        TRACE("Hanshake failed on pin %d\n", pin);
        return PROTOCOL_ERROR;
    }

    // read 40 bits into buf
    for (unsigned i = 0;  i < 40;  ++i) {
        if (wait_while(pin, 0, DATA_RECV_LOW_US) == TIMEOUT) {
            TRACE("Timed out waiting to recieve data low signal\n");
            return PROTOCOL_ERROR;
        }
        int try_one = wait_while(pin, 1, DATA_RECV_HIGH_ONE_US);
        switch (try_one) {
            case TIMEOUT:
                // pin was high for at least DATA_RECV_HIGH_ONE_US; it has to be a 1
                buf[i / 8] |= (1 << (7 - (i % 8)));
                break;
            default:
                // it could be a 0, if the pin was high for less than DATA_RECV_HIGH_ZERO_US
                // otherwise, it's a 1
                if (DATA_RECV_HIGH_ZERO_US < try_one) {
                    buf[i / 8] |= (1 << (7 - (i % 8)));
                }
                break;
        }
    }

    return 0;
}


static term nif_dht_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term pin = argv[0];
    VALIDATE_VALUE(pin, term_is_integer);

    if (UNLIKELY(memory_ensure_free(ctx, 20) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    uint8_t buf[5];
    memset(buf, 0, 5);
    int err = read_into(term_to_int(pin), buf);
    if (err == PROTOCOL_ERROR) {
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, context_make_atom(ctx, dht_bad_read));
            return error_tuple;
        }
    } else {
        term ok_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, term_from_literal_binary(buf, 5, ctx));
        return ok_tuple;
    }
}


static const struct Nif dht_read_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_dht_read
};

void atomvm_dht_init(GlobalContext *global)
{
    // no-op
}

const struct Nif *atomvm_dht_get_nif(const char *nifname)
{
    if (strcmp("dht:read/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &dht_read_nif;
    }
    return NULL;
}
