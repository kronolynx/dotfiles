import QtQuick
import qs.theme

// One coloured pill inside an island: a Nerd Font glyph, an optional label, and
// mouse handling. Mirrors the shared `defaults` block in sketchybarrc.
Rectangle {
    id: root

    property string icon: ""
    property string label: ""
    property color cellColor: Config.dgrey
    property color fgColor: Config.black
    property color iconColor: fgColor
    property int iconWidth: 25 // sketchybar icon.width=25
    property bool showLabel: label !== ""
    readonly property bool hovered: mouse.containsMouse

    // Extra items (a slider, tray icons) are appended after the label.
    default property alias content: layout.data

    signal clicked
    signal rightClicked
    signal middleClicked
    signal scrolled(int delta)

    implicitHeight: Config.cellHeight
    implicitWidth: layout.implicitWidth
    radius: Config.radius
    color: cellColor

    Behavior on color {
        ColorAnimation {
            duration: 120
        }
    }

    // Declared before the content so that interactive children (a volume
    // slider, tray icons) sit above it and get the clicks first; plain Text
    // does not accept mouse events, so everything else falls through to here.
    MouseArea {
        id: mouse

        anchors.fill: parent
        hoverEnabled: true
        acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton

        onClicked: event => {
            if (event.button === Qt.RightButton)
                root.rightClicked();
            else if (event.button === Qt.MiddleButton)
                root.middleClicked();
            else
                root.clicked();
        }

        onWheel: event => {
            root.scrolled(event.angleDelta.y > 0 ? 1 : -1);
        }
    }

    Row {
        id: layout

        height: parent.height
        spacing: 0

        Text {
            // Careful: size off the source condition, never off `visible`.
            // `visible` reports *effective* visibility, so reading it here ties
            // the cell's width to its parents' and the whole chain latches at 0.
            visible: root.icon !== ""
            width: root.icon !== "" ? Math.max(root.iconWidth, implicitWidth + Config.iconPad * 2) : 0
            height: parent.height
            horizontalAlignment: Text.AlignHCenter
            verticalAlignment: Text.AlignVCenter
            text: root.icon
            color: root.iconColor
            font.family: Config.iconFont
            font.pixelSize: Config.iconSize
            font.bold: true
        }

        Text {
            // No width binding: `width: implicitWidth` on a Text is a binding
            // loop, and Row already leaves invisible children out of its
            // layout, so hiding the label collapses the cell on its own.
            visible: root.showLabel
            height: parent.height
            verticalAlignment: Text.AlignVCenter
            rightPadding: Config.labelPad
            leftPadding: root.icon !== "" ? 0 : Config.labelPad
            text: root.label
            color: root.fgColor
            font.family: Config.labelFont
            font.pixelSize: Config.labelSize
            font.weight: Font.DemiBold
        }
    }
}
