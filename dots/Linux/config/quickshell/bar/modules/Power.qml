import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland
import qs.theme
import qs.bar

// Session menu, mirroring items/apple.sh on the Mac: an icon on the left with
// a popup of system actions.
//
// The reversible actions fire on a single click. Logging out, rebooting and
// powering off ask for a second click first -- a stray click in a small menu
// should not end the session.
Cell {
    id: root

    property string confirming: ""

    readonly property var actions: [
        {
            "id": "lock",
            "name": "Lock",
            "icon": Icons.lock,
            "color": Config.blue,
            "confirm": false
        },
        {
            "id": "suspend",
            "name": "Suspend",
            "icon": Icons.suspend,
            "color": Config.cyan,
            "confirm": false
        },
        {
            "id": "hibernate",
            "name": "Hibernate",
            "icon": Icons.hibernate,
            "color": Config.magenta,
            "confirm": false
        },
        {
            "id": "logout",
            "name": "Log out",
            "icon": Icons.logout,
            "color": Config.yellow,
            "confirm": true
        },
        {
            "id": "reboot",
            "name": "Reboot",
            "icon": Icons.restart,
            "color": Config.orange,
            "confirm": true
        },
        {
            "id": "poweroff",
            "name": "Shut down",
            "icon": Icons.power,
            "color": Config.red,
            "confirm": true
        }
    ]

    function run(action) {
        if (action.confirm && root.confirming !== action.id) {
            root.confirming = action.id;
            return;
        }
        root.confirming = "";
        popup.showing = false;

        switch (action.id) {
        case "lock":
            proc.command = ["hyprlock"];
            proc.running = true;
            break;
        case "suspend":
            proc.command = ["systemctl", "suspend"];
            proc.running = true;
            break;
        case "hibernate":
            proc.command = ["systemctl", "hibernate"];
            proc.running = true;
            break;
        case "reboot":
            proc.command = ["systemctl", "reboot"];
            proc.running = true;
            break;
        case "poweroff":
            proc.command = ["systemctl", "poweroff"];
            proc.running = true;
            break;
        case "logout":
            // Lua form, because a lua config makes Hyprland evaluate dispatch
            // strings as lua and reject the classic "exit".
            Hyprland.dispatch("hl.dsp.exit()");
            break;
        }
    }

    icon: Icons.power
    iconColor: Config.red
    cellColor: Config.dgrey
    onClicked: popup.showing = !popup.showing

    Process {
        id: proc

        stderr: StdioCollector {
            onStreamFinished: {
                if (text.trim() !== "")
                    console.warn("power:", text.trim());
            }
        }
    }

    Popup {
        id: popup

        anchorItem: root
        implicitWidth: 190
        implicitHeight: col.implicitHeight + Config.popupPad * 2

        onShowingChanged: {
            if (!showing)
                root.confirming = "";
        }

        Column {
            id: col

            width: parent.width
            spacing: 2

            Repeater {
                model: root.actions

                Rectangle {
                    id: actionRow

                    required property var modelData

                    readonly property bool armed: root.confirming === modelData.id

                    width: parent.width
                    height: 26
                    radius: Config.radius
                    color: actionRow.armed ? Qt.rgba(Config.red.r, Config.red.g, Config.red.b, 0.25) : (mouse.containsMouse ? Config.rowHover : "transparent")

                    Row {
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: 8
                        leftPadding: 6

                        Text {
                            width: 18
                            anchors.verticalCenter: parent.verticalCenter
                            text: actionRow.modelData.icon
                            color: actionRow.modelData.color
                            font.family: Config.iconFont
                            font.pixelSize: Config.iconSize
                        }

                        Text {
                            anchors.verticalCenter: parent.verticalCenter
                            text: actionRow.armed ? "Click to confirm" : actionRow.modelData.name
                            color: actionRow.armed ? Config.red : Config.white
                            font.family: Config.labelFont
                            font.pixelSize: Config.labelSize
                            font.bold: actionRow.armed
                        }
                    }

                    MouseArea {
                        id: mouse

                        anchors.fill: parent
                        hoverEnabled: true
                        onClicked: root.run(actionRow.modelData)
                        // Moving away from an armed row disarms it, so a
                        // confirmation cannot sit waiting for a stray click.
                        onExited: {
                            if (actionRow.armed)
                                root.confirming = "";
                        }
                    }
                }
            }
        }
    }
}
