import QtQuick
import QtQuick.Controls
import Quickshell
import Quickshell.Bluetooth
import qs.theme
import qs.bar

// No sketchybar equivalent either. bluez is installed with no manager, so this
// cell is the only bluetooth UI on the machine.
Cell {
    id: root

    readonly property var adapter: Bluetooth.defaultAdapter
    readonly property bool enabled: root.adapter?.enabled ?? false
    readonly property var devices: root.adapter?.devices?.values ?? []
    readonly property var connected: root.devices.filter(d => d.connected)

    // Connected first, then paired, then whatever discovery turns up, and by
    // name within each group so rows do not jump about as RSSI changes.
    readonly property var listed: [...root.devices].sort((a, b) => {
        if (a.connected !== b.connected)
            return a.connected ? -1 : 1;
        if (a.paired !== b.paired)
            return a.paired ? -1 : 1;
        return (a.name ?? a.address ?? "").localeCompare(b.name ?? b.address ?? "");
    })

    icon: {
        if (!root.enabled)
            return Icons.bluetoothOff;
        return root.connected.length > 0 ? Icons.bluetoothConnected : Icons.bluetooth;
    }
    label: root.connected.length > 1 ? `${root.connected.length}` : ""
    cellColor: Config.grey
    onClicked: popup.showing = !popup.showing
    onRightClicked: {
        if (root.adapter)
            root.adapter.enabled = !root.adapter.enabled;
    }

    // Only scan while the popup is open.
    Binding {
        target: root.adapter
        property: "discovering"
        value: popup.showing && root.enabled
        when: root.adapter !== null
    }

    Popup {
        id: popup

        anchorItem: root
        implicitWidth: 300
        implicitHeight: col.implicitHeight + Config.popupPad * 2

        Column {
            id: col

            width: parent.width
            spacing: 6

            Row {
                width: parent.width
                spacing: 8

                Text {
                    width: parent.width - toggle.width - 8
                    text: root.enabled ? (root.adapter?.name ?? "Bluetooth") : "Bluetooth off"
                    elide: Text.ElideRight
                    color: Config.white
                    font.family: Config.labelFont
                    font.pixelSize: Config.labelSize
                    font.bold: true
                }

                Rectangle {
                    id: toggle

                    width: 40
                    height: 18
                    radius: 9
                    color: root.enabled ? Config.blue : Config.dgrey

                    Rectangle {
                        x: root.enabled ? parent.width - width - 2 : 2
                        y: 2
                        width: 14
                        height: 14
                        radius: 7
                        color: Config.white

                        Behavior on x {
                            NumberAnimation {
                                duration: 120
                            }
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: {
                            if (root.adapter)
                                root.adapter.enabled = !root.adapter.enabled;
                        }
                    }
                }
            }

            Text {
                visible: root.adapter === null
                text: "bluetooth service not running"
                color: Config.grey
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize - 1
            }

            Text {
                visible: root.adapter?.discovering ?? false
                text: "Scanning…"
                color: Config.grey
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize - 1
            }

            Rectangle {
                width: parent.width
                height: 1
                visible: root.listed.length > 0
                color: Config.dgrey
            }

            // Scrollable, and a diffed model: discovery keeps updating these
            // objects, and rebuilding the rows under the pointer would swallow
            // the click -- the same bug the wifi list had.
            Flickable {
                width: parent.width
                height: Math.min(rows.implicitHeight, 264)
                contentWidth: width
                contentHeight: rows.implicitHeight
                clip: true
                boundsBehavior: Flickable.StopAtBounds

                ScrollBar.vertical: ScrollBar {
                    policy: rows.implicitHeight > 264 ? ScrollBar.AlwaysOn : ScrollBar.AlwaysOff
                }

                Column {
                    id: rows

                    width: parent.width
                    spacing: 1

                    Repeater {
                        model: ScriptModel {
                            values: root.listed
                        }

                        Rectangle {
                            id: deviceRow

                            required property var modelData

                            width: rows.width
                            height: 28
                            radius: Config.radius
                            color: hover.containsMouse ? Config.rowHover : "transparent"

                            Row {
                                anchors.verticalCenter: parent.verticalCenter
                                anchors.left: parent.left
                                anchors.right: parent.right
                                anchors.leftMargin: 6
                                anchors.rightMargin: 6
                                spacing: 8

                                Text {
                                    anchors.verticalCenter: parent.verticalCenter
                                    width: parent.width - 70
                                    text: deviceRow.modelData.name ?? deviceRow.modelData.address
                                    elide: Text.ElideRight
                                    color: deviceRow.modelData.connected ? Config.white : Config.grey
                                    font.family: Config.labelFont
                                    font.pixelSize: Config.labelSize
                                    font.bold: deviceRow.modelData.connected
                                }

                                Text {
                                    anchors.verticalCenter: parent.verticalCenter
                                    visible: deviceRow.modelData.batteryAvailable
                                    text: `${Math.round((deviceRow.modelData.battery ?? 0) * 100)}%`
                                    color: Config.grey
                                    font.family: Config.labelFont
                                    font.pixelSize: Config.labelSize - 2
                                }

                                Text {
                                    anchors.verticalCenter: parent.verticalCenter
                                    visible: deviceRow.modelData.pairing || deviceRow.modelData.connected
                                    text: deviceRow.modelData.pairing ? "…" : "•"
                                    color: Config.blue
                                    font.family: Config.labelFont
                                    font.pixelSize: Config.labelSize
                                }
                            }

                            MouseArea {
                                id: hover

                                anchors.fill: parent
                                hoverEnabled: true
                                acceptedButtons: Qt.LeftButton | Qt.RightButton

                                onClicked: event => {
                                    const device = deviceRow.modelData;
                                    if (event.button === Qt.RightButton) {
                                        if (device.paired)
                                            device.forget();
                                        return;
                                    }
                                    if (device.connected)
                                        device.disconnect();
                                    else if (device.paired)
                                        device.connect();
                                    else
                                        device.pair();
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
