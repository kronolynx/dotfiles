pragma Singleton

import Quickshell
import Quickshell.Io

// swaync's panel state. The Mac version of this cell watched Slack's dock
// badge; on Linux the notification centre's own state is the analogue.
Singleton {
    id: root

    property int count: 0
    property bool dnd: false
    property bool panelOpen: false

    function toggle() {
        toggleProc.running = true;
    }

    function toggleDnd() {
        dndProc.running = true;
    }

    Process {
        id: toggleProc

        command: ["swaync-client", "-t", "-sw"]
    }

    Process {
        id: dndProc

        command: ["swaync-client", "-d", "-sw"]
    }

    // swaync-client --subscribe streams a JSON line on every state change, so
    // there is nothing to poll.
    Process {
        running: true
        command: ["swaync-client", "--subscribe"]

        stdout: SplitParser {
            onRead: line => {
                try {
                    const state = JSON.parse(line);
                    root.count = state.count ?? 0;
                    root.dnd = state.dnd ?? false;
                    root.panelOpen = state.visible ?? false;
                } catch (e) {
                    // swaync prints the odd non-JSON line; ignore it
                }
            }
        }
    }
}
