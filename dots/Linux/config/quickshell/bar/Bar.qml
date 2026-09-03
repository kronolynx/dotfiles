import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import QtQuick
import qs.theme
import qs.bar.modules

// The bar itself: a transparent full-width strip holding two floating islands,
// exactly like sketchybar's bar (height=29, y_offset=4, margin=5) with its
// spaces bracket on the left and status bracket on the right.
PanelWindow {
    id: root

    required property var modelData

    screen: modelData
    color: "transparent"
    implicitHeight: Config.barSpace
    exclusiveZone: Config.barSpace

    WlrLayershell.namespace: "quickshell:bar"
    // The bar never takes keyboard focus, so clicking a cell cannot pull focus
    // off the focused window. The one thing that needs typing -- the wifi
    // password -- is its own overlay surface (bar/PasswordPrompt.qml).
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.None

    anchors {
        top: true
        left: true
        right: true
    }

    // Hyprland's workspace list needs one explicit refresh; the event socket
    // only reports changes from then on.
    Component.onCompleted: {
        Hyprland.refreshWorkspaces();
        Hyprland.refreshToplevels();
    }

    // Workspaces that Quickshell learns about from an event rather than from a
    // `j/workspaces` fetch can be left with id -1 and no monitor, which breaks
    // both activate() and the per-monitor filter. Re-fetch when the set of
    // workspaces changes; these events are rare.
    Connections {
        target: Hyprland

        function onRawEvent(event) {
            if (event.name.includes("workspace"))
                Hyprland.refreshWorkspaces();
        }
    }

    Item {
        anchors.fill: parent
        anchors.topMargin: Config.barOffset
        anchors.leftMargin: Config.barMargin
        anchors.rightMargin: Config.barMargin

        Island {
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter

            // Leftmost, where the Mac keeps its apple menu.
            Power {}

            Workspaces {
                screenName: root.modelData?.name ?? ""
            }
        }

        Island {
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter

            // Left to right, matching the Mac: the new Linux-only cells sit
            // ahead of the bell, then battery, volume, calendar, clock.
            Tray {}

            Brightness {}

            PowerProfile {}

            Wifi {}

            Bluetooth {}

            Notifications {}

            Battery {}

            Volume {}

            Calendar {}

            Clock {}
        }
    }
}
