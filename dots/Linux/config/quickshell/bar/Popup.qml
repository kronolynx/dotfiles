import Quickshell
import Quickshell.Hyprland
import QtQuick
import qs.theme

// Panel hanging off a cell. Callers set implicitWidth/implicitHeight from their
// own content (never from a child that fills this, or the size binds in a loop).
PopupWindow {
    id: root

    property Item anchorItem
    property bool showing: false

    default property alias content: inner.data

    anchor.item: anchorItem
    anchor.edges: Edges.Bottom
    anchor.gravity: Edges.Bottom
    anchor.adjustment: PopupAdjustment.SlideX
    anchor.margins.top: Config.islandPad

    visible: showing
    color: "transparent"

    Rectangle {
        anchors.fill: parent
        radius: Config.popupRadius
        color: Config.popupBg
        border.width: 1
        border.color: Config.dgrey

        Item {
            id: inner

            anchors.fill: parent
            anchors.margins: Config.popupPad
        }
    }

    // Click anywhere else and the popup goes away.
    HyprlandFocusGrab {
        windows: [root]
        active: root.showing
        onCleared: root.showing = false
    }
}
