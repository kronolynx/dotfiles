import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import QtQuick
import qs.theme

// Wifi password dialog.
//
// This is a PanelWindow rather than a Popup on purpose: a PopupWindow never
// receives keyboard input under Hyprland. Keyboard focus goes to the layer
// surface, and an xdg-popup hanging off it gets nothing -- tested with
// synthetic keys, with both OnDemand and Exclusive focus, and the field stayed
// empty while Qt happily reported activeFocus: true. A layer surface with
// Exclusive focus does receive keys.
PanelWindow {
    id: root

    // Empty ssid means hidden.
    property string ssid: ""
    property bool known: false
    property bool reveal: false

    signal submitted(string password)
    signal cancelled

    function open(networkName, isKnown) {
        root.ssid = networkName;
        root.known = isKnown;
        root.reveal = false;
        field.text = "";
        field.forceActiveFocus();
    }

    function close() {
        field.text = "";
        root.ssid = "";
        root.reveal = false;
    }

    visible: ssid !== ""
    color: "transparent"
    implicitWidth: 320
    implicitHeight: col.implicitHeight + Config.popupPad * 2

    anchors {
        top: true
        right: true
    }
    margins {
        top: Config.barSpace
        right: Config.barMargin
    }

    // Above the bar, and reserving nothing.
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:wifi-password"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
    exclusionMode: ExclusionMode.Ignore

    Rectangle {
        anchors.fill: parent
        radius: Config.popupRadius
        color: Config.popupBg
        border.width: 1
        border.color: Config.orange

        Column {
            id: col

            x: Config.popupPad
            y: Config.popupPad
            width: parent.width - Config.popupPad * 2
            spacing: 6

            Text {
                width: parent.width
                text: `Password for ${root.ssid}`
                elide: Text.ElideRight
                color: Config.white
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize
                font.bold: true
            }

            Rectangle {
                width: parent.width
                height: 28
                radius: Config.radius
                color: Config.black
                border.width: 1
                border.color: field.activeFocus ? Config.orange : Config.dgrey

                TextInput {
                    id: field

                    anchors.left: parent.left
                    anchors.right: revealButton.left
                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    anchors.leftMargin: 6
                    anchors.rightMargin: 4
                    verticalAlignment: TextInput.AlignVCenter
                    echoMode: root.reveal ? TextInput.Normal : TextInput.Password
                    focus: true
                    color: Config.white
                    font.family: Config.labelFont
                    font.pixelSize: Config.labelSize
                    selectByMouse: true

                    onAccepted: {
                        if (text !== "")
                            root.submitted(text);
                    }

                    Keys.onEscapePressed: root.cancelled()
                }

                // Show/hide the password.
                Text {
                    id: revealButton

                    anchors.right: parent.right
                    anchors.rightMargin: 4
                    anchors.verticalCenter: parent.verticalCenter
                    width: 22
                    horizontalAlignment: Text.AlignHCenter
                    text: root.reveal ? Icons.eyeOff : Icons.eye
                    color: eyeArea.containsMouse ? Config.orange : Config.grey
                    font.family: Config.iconFont
                    font.pixelSize: Config.iconSize

                    MouseArea {
                        id: eyeArea

                        anchors.fill: parent
                        anchors.margins: -4
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            root.reveal = !root.reveal;
                            // Keep typing where it was.
                            field.forceActiveFocus();
                        }
                    }
                }
            }

            Text {
                width: parent.width
                text: "Enter to connect, Esc to cancel. The password is saved to the connection so it is not asked again."
                wrapMode: Text.Wrap
                color: Config.grey
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize - 2
            }
        }
    }

    // Clicking elsewhere cancels.
    HyprlandFocusGrab {
        windows: [root]
        active: root.visible
        onCleared: root.cancelled()
    }
}
