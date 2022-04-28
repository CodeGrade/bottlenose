/*!
 * @module jquery.keyDecoder
 * @link http://github.com/dvdln/jquery.keyDecoder
 * @version 1.1
 * @author David Lane (http://about.me/davidlane)
 * @license MIT (http://en.wikipedia.org/wiki/MIT_License)
 * @copyright 2012 David Lane, all rights reserved
 */

/*global jQuery*/
(function ($) {
    'use strict';

    var KEY_SHIFT = 16,
        KEY_CTRL  = 17,
        KEY_ALT   = 18;

    $.keyDecoder = {
        keyboardEventTypes: [
            'keydown',
            'keypress',
            'keyup'
        ],

        modifiers: [
            KEY_SHIFT,
            KEY_CTRL,
            KEY_ALT
        ],

        specialKeys: {
            8:   'Backspace',
            9:   'Tab',
            13:  'Return',
            16:  'Shift',
            17:  'Ctrl',
            18:  'Alt',
            19:  'Pause',
            20:  'CapsLock',
            27:  'Esc',
            32:  'Space',
            33:  'PageUp',
            34:  'PageDown',
            35:  'End',
            36:  'Home',
            37:  'Left',
            38:  'Up',
            39:  'Right',
            40:  'Down',
            45:  'Insert',
            46:  'Del',
            96:  '0',
            97:  '1',
            98:  '2',
            99:  '3',
            100: '4',
            101: '5',
            102: '6',
            103: '7',
            104: '8',
            105: '9',
            106: '*',
            107: '+',
            109: '-',
            110: '.',
            111: '/',
            112: 'F1',
            113: 'F2',
            114: 'F3',
            115: 'F4',
            116: 'F5',
            117: 'F6',
            118: 'F7',
            119: 'F8',
            120: 'F9',
            121: 'F10',
            122: 'F11',
            123: 'F12',
            144: 'NumLock',
            145: 'Scroll',
            191: '/',
            224: 'Meta'
        },

        shiftKeys: {
            '`':  '~',
            '1':  '!',
            '2':  '@',
            '3':  '#',
            '4':  '$',
            '5':  '%',
            '6':  '^',
            '7':  '&',
            '8':  '*',
            '9':  '(',
            '0':  ')',
            '-':  '_',
            '=':  '+',
            ';':  ': ',
            '\'': '\"',
            ',':  '<',
            '.':  '>',
            '/':  '?',
            '\\': '|'
        },

        /**
         * Translates an object to a hotkey string.
         * @param {object}
         */
        parse: function (obj) {
            if ($.isNumeric(obj)) {
                return this.parseKeyCode.apply(this, arguments);
            }

            if (this.isKeyboardEvent(obj)) {
                return this.parseKeyboardEvent.apply(this, arguments);
            }

            return '';
        },

        /**
         * Converts a character code to string.
         * @param {string} Character code
         * @return {string}
         */
        parseKeyCode: function (code) {
            return this.specialKeys[code] || String.fromCharCode(code).toUpperCase();
        },

        /**
         * Translates a keyboard event to a string.
         * @param {object} Keyboard event object
         * @param {boolean} Apply shift modifier to key
         * @return {string}
         */
        parseKeyboardEvent: function (event, translateShiftKeys) {
            var modifier = '',
                code = event.which,
                key = this.parseKeyCode(code);

            // If the keypress is a modifier then don't prepend it
            if (!this.isModifier(code)) {
                if (event.altKey) {
                    modifier += this.parseKeyCode(KEY_ALT) + '+';
                }

                if (event.ctrlKey) {
                    modifier += this.parseKeyCode(KEY_CTRL) + '+';
                }

                if (event.shiftKey) {
                    if (translateShiftKeys === true && this.shiftKeys[key]) {
                        key = this.shiftKeys[key];
                    } else {
                        modifier += this.parseKeyCode(KEY_SHIFT) + '+';
                    }
                }
            }

            return modifier + key;
        },

        /**
         * Determines if a keycode is for a modifier key
         * @param {number} Keycode
         * @return {boolean}
         */
        isModifier: function (code) {
            return ($.inArray(code, this.modifiers) > -1);
        },

        /**
         * Determines if an object is a keyboard event.
         * @param {object} Event
         */
        isKeyboardEvent: function (event) {
            return (event && event.type && $.inArray(event.type, this.keyboardEventTypes) > -1);
        }
    };

}(jQuery));
