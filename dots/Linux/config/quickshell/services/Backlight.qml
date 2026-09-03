pragma Singleton

import Quickshell
import Quickshell.Io
// for Timer
import QtQuick
import qs.theme

// Screen brightness: read from sysfs, written through logind.
//
// Writing needs help: /sys/class/backlight/*/brightness is root-owned and this
// user is not in the `video` group, so a direct write is denied. logind's
// SetBrightness does it without root and without any extra package, which is
// why it is preferred over brightnessctl here -- brightnessctl is still worth
// installing for the XF86MonBrightness binds in hypr/keybinds.lua.
Singleton {
    id: root

    readonly property string device: Config.backlightDevice
    property int max: 0
    property int raw: 0

    // Nothing is known until max_brightness has been read.
    readonly property bool ready: max > 0

    // 0-1, matching how UPower and Networking report their fractions
    readonly property real value: ready ? raw / max : 0
    readonly property int percent: Math.round(value * 100)

    // Pending target while dragging, so a drag does not spawn one process per
    // mouse move. -1 means nothing in flight.
    property int pendingPercent: -1

    function setPercent(pct) {
        root.pendingPercent = Math.round(Math.max(1, Math.min(100, pct)));
        // Reflect it immediately; sysfs confirms a moment later.
        if (root.ready)
            root.raw = Math.round(root.pendingPercent / 100 * root.max);
        if (!throttle.running)
            throttle.start();
    }

    function adjust(delta) {
        if (!root.ready)
            return;
        root.setPercent((root.pendingPercent >= 0 ? root.pendingPercent : root.percent) + delta);
    }

    function flush() {
        if (root.pendingPercent < 0)
            return;
        // logind wants an absolute value, so max has to be known. Retry rather
        // than drop the request: dropping it used to leave pendingPercent set,
        // which then blocked sysfs from ever syncing again.
        if (!root.ready) {
            throttle.restart();
            return;
        }
        const target = Math.max(1, Math.round(root.pendingPercent / 100 * root.max));
        root.pendingPercent = -1;
        setProc.command = ["busctl", "call", "org.freedesktop.login1", "/org/freedesktop/login1/session/auto", "org.freedesktop.login1.Session", "SetBrightness", "ssu", "backlight", root.device, `${target}`];
        setProc.running = true;
    }

    Timer {
        id: throttle

        interval: 60
        onTriggered: root.flush()
    }

    FileView {
        // Read up front: the slider and the scroll handler are useless until
        // max is known, and an async read leaves them at 0 on startup.
        path: `/sys/class/backlight/${root.device}/max_brightness`
        preload: true
        onLoaded: root.max = parseInt(text())
    }

    FileView {
        id: current

        path: `/sys/class/backlight/${root.device}/brightness`
        preload: true
        watchChanges: true
        onFileChanged: reload()
        onLoaded: {
            // Ignore sysfs while a write is in flight, or the slider fights the
            // value it is on its way to setting.
            if (root.pendingPercent < 0 && !setProc.running)
                root.raw = parseInt(text());
        }
    }

    Process {
        id: setProc

        stderr: StdioCollector {
            onStreamFinished: {
                if (text.trim() !== "")
                    console.warn("brightness:", text.trim());
            }
        }

        onExited: {
            current.reload();
            // Coalesce anything that arrived while this call was running.
            if (root.pendingPercent >= 0)
                root.flush();
        }
    }
}
