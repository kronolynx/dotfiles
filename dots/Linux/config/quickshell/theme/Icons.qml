pragma Singleton

import Quickshell

// Nerd Font glyphs standing in for the SF Symbols used on the Mac.
//
// Every codepoint here was rendered and eyeballed before being picked, and all
// of them sit inside the ranges JetBrainsMono Nerd Font actually ships
// (f000-f385 Font Awesome, f0001-f1af0 Material Design).
//
// They are written as codepoints rather than literal characters on purpose:
// these live in the Unicode private use area, where a stray editor or a
// mis-guessed file encoding silently turns them into mojibake.
Singleton {
    id: root

    // clock + calendar are the exact glyphs sketchybar's icons.sh uses
    readonly property string clock: String.fromCodePoint(0xf1452)     // md-clock-time-four-outline
    readonly property string calendar: String.fromCodePoint(0xf073)   // fa-calendar

    // wifi: md-wifi-strength-1 .. -4, then -alert and -off
    readonly property var wifiBars: [
        String.fromCodePoint(0xf091f),
        String.fromCodePoint(0xf0922),
        String.fromCodePoint(0xf0925),
        String.fromCodePoint(0xf0928)
    ]
    readonly property string wifiAlert: String.fromCodePoint(0xf092b) // on, not connected
    readonly property string wifiOff: String.fromCodePoint(0xf092e)

    // battery: horizontal fa glyphs, matching the shape of the Mac's SF battery
    readonly property string batteryFull: String.fromCodePoint(0xf240)
    readonly property string battery75: String.fromCodePoint(0xf241)
    readonly property string battery50: String.fromCodePoint(0xf242)
    readonly property string battery25: String.fromCodePoint(0xf243)
    readonly property string battery0: String.fromCodePoint(0xf244)
    readonly property string charging: String.fromCodePoint(0xf0e7)   // fa-bolt

    // volume: md-volume-high / -medium / -low / -off
    readonly property string volumeHigh: String.fromCodePoint(0xf057e)
    readonly property string volumeMid: String.fromCodePoint(0xf0580)
    readonly property string volumeLow: String.fromCodePoint(0xf057f)
    readonly property string volumeMuted: String.fromCodePoint(0xf0581)

    readonly property string bell: String.fromCodePoint(0xf009a)
    readonly property string bellUnread: String.fromCodePoint(0xf0d59)
    readonly property string bellOff: String.fromCodePoint(0xf009b)

    readonly property string bluetooth: String.fromCodePoint(0xf00af)
    readonly property string bluetoothConnected: String.fromCodePoint(0xf00b0)
    readonly property string bluetoothOff: String.fromCodePoint(0xf00b2)

    readonly property string brightness: String.fromCodePoint(0xf00e0)

    readonly property string profileSaver: String.fromCodePoint(0xf032a)       // md-leaf
    readonly property string profileBalanced: String.fromCodePoint(0xf04c5)    // md-speedometer
    readonly property string profilePerformance: String.fromCodePoint(0xf0241) // md-flash

    readonly property string lock: String.fromCodePoint(0xf0341)               // md-lock
    readonly property string refresh: String.fromCodePoint(0xf0450)            // md-refresh
    readonly property string eye: String.fromCodePoint(0xf0208)                // md-eye
    readonly property string eyeOff: String.fromCodePoint(0xf0209)             // md-eye-off
    readonly property string chevronLeft: String.fromCodePoint(0xf0141)        // md-chevron-left
    readonly property string chevronRight: String.fromCodePoint(0xf0142)       // md-chevron-right

    // session / power
    readonly property string power: String.fromCodePoint(0xf0425)              // md-power
    readonly property string restart: String.fromCodePoint(0xf0709)            // md-restart
    readonly property string logout: String.fromCodePoint(0xf0343)             // md-logout
    readonly property string suspend: String.fromCodePoint(0xf04b2)            // md-sleep
    readonly property string hibernate: String.fromCodePoint(0xf0717)          // md-snowflake

    // ---- bucket helpers, thresholds copied from the sketchybar plugins ----

    // plugins/battery.sh: 90+ / 60-89 / 30-59 / 10-29 / rest
    function battery(pct, charging) {
        if (charging)
            return root.charging;
        if (pct >= 90)
            return root.batteryFull;
        if (pct >= 60)
            return root.battery75;
        if (pct >= 30)
            return root.battery50;
        if (pct >= 10)
            return root.battery25;
        return root.battery0;
    }

    // plugins/volume.sh: 60+ / 30-59 / 1-29 / 0
    function volume(pct, muted) {
        if (muted || pct <= 0)
            return root.volumeMuted;
        if (pct >= 60)
            return root.volumeHigh;
        if (pct >= 30)
            return root.volumeMid;
        return root.volumeLow;
    }

    // strength is a 0-1 fraction, as Quickshell.Networking reports it
    function wifi(strength) {
        const i = Math.min(3, Math.max(0, Math.floor(strength * 4)));
        return root.wifiBars[i];
    }
}
