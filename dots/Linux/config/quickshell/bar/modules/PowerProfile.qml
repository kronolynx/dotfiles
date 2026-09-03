import QtQuick
import Quickshell.Services.UPower
import qs.theme
import qs.bar

// power-profiles-daemon. No sketchybar equivalent: on the Mac this lives in the
// macOS battery menu. The cell stays neutral and the glyph carries the state.
Cell {
    id: root

    readonly property var profiles: [
        {
            "value": PowerProfile.PowerSaver,
            "name": "Power saver",
            "icon": Icons.profileSaver,
            "color": Config.green
        },
        {
            "value": PowerProfile.Balanced,
            "name": "Balanced",
            "icon": Icons.profileBalanced,
            "color": Config.white
        },
        {
            "value": PowerProfile.Performance,
            "name": "Performance",
            "icon": Icons.profilePerformance,
            "color": Config.yellow
        }
    ]
    readonly property var active: profiles.find(p => p.value === PowerProfiles.profile) ?? profiles[1]

    icon: root.active.icon
    iconColor: root.active.color
    cellColor: Config.dgrey
    onClicked: popup.showing = !popup.showing
    // Scroll straight through the profiles without opening the popup.
    onScrolled: delta => {
        const i = root.profiles.findIndex(p => p.value === PowerProfiles.profile);
        const next = Math.max(0, Math.min(root.profiles.length - 1, i + delta));
        PowerProfiles.profile = root.profiles[next].value;
    }

    Popup {
        id: popup

        anchorItem: root
        implicitWidth: 170
        implicitHeight: col.implicitHeight + Config.popupPad * 2

        Column {
            id: col

            width: parent.width
            spacing: 2

            Repeater {
                model: root.profiles

                Rectangle {
                    required property var modelData

                    width: parent.width
                    height: 26
                    radius: Config.radius
                    color: mouse.containsMouse ? Config.rowHover : "transparent"

                    Row {
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: 8
                        leftPadding: 6

                        Text {
                            width: 18
                            text: modelData.icon
                            color: modelData.color
                            font.family: Config.iconFont
                            font.pixelSize: Config.iconSize
                        }

                        Text {
                            text: modelData.name
                            color: PowerProfiles.profile === modelData.value ? Config.white : Config.grey
                            font.family: Config.labelFont
                            font.pixelSize: Config.labelSize
                            font.bold: PowerProfiles.profile === modelData.value
                        }
                    }

                    MouseArea {
                        id: mouse

                        anchors.fill: parent
                        hoverEnabled: true
                        onClicked: {
                            PowerProfiles.profile = modelData.value;
                            popup.showing = false;
                        }
                    }
                }
            }
        }
    }
}
