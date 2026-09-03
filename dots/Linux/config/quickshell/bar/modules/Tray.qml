import QtQuick
import Quickshell
import Quickshell.Services.SystemTray
import qs.theme
import qs.bar

// StatusNotifier host. There is no menu bar on Linux to fall back to, so
// without this Slack/Discord/Steam have nowhere to put their icons.
Cell {
    id: root

    readonly property var items: SystemTray.items.values.filter(i => i.status !== Status.Passive)

    icon: ""
    cellColor: Config.dgrey
    // Collapse the cell entirely when nothing is in the tray. The width keys
    // off the item count, not off `visible`, which is parent-dependent.
    visible: root.items.length > 0
    implicitWidth: root.items.length > 0 ? icons.implicitWidth + Config.iconPad * 2 : 0

    Row {
        id: icons

        x: Config.iconPad
        height: parent.height
        spacing: 8

        Repeater {
            model: root.items

            Item {
                required property var modelData

                width: 16
                height: parent.height

                Image {
                    anchors.centerIn: parent
                    width: 16
                    height: 16
                    source: modelData.icon
                    sourceSize.width: 32
                    sourceSize.height: 32
                    smooth: true
                    opacity: mouse.containsMouse ? 0.75 : 1
                }

                MouseArea {
                    id: mouse

                    anchors.fill: parent
                    hoverEnabled: true
                    acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton

                    onClicked: event => {
                        if (event.button === Qt.RightButton || modelData.onlyMenu) {
                            if (modelData.hasMenu)
                                menu.open();
                        } else if (event.button === Qt.MiddleButton) {
                            modelData.secondaryActivate();
                        } else {
                            modelData.activate();
                        }
                    }
                }

                QsMenuAnchor {
                    id: menu

                    menu: modelData.menu
                    anchor.item: parent
                    anchor.edges: Edges.Bottom
                    anchor.gravity: Edges.Bottom
                }
            }
        }
    }
}
