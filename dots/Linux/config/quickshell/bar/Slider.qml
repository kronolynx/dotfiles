import QtQuick
import qs.theme

// Minimal slider styled like sketchybar's (3px track, round knob), rather than
// fighting QtQuick.Controls' theming.
Item {
    id: root

    property real value: 0 // 0-1
    property color trackColor: Config.dgrey
    property color fillColor: Config.white

    signal moved(real value)

    function clamp(v) {
        return Math.max(0, Math.min(1, v));
    }

    implicitHeight: 14
    implicitWidth: 100

    Rectangle {
        anchors.verticalCenter: parent.verticalCenter
        width: parent.width
        height: 3
        radius: height / 2
        color: root.trackColor

        Rectangle {
            width: parent.width * root.clamp(root.value)
            height: parent.height
            radius: parent.radius
            color: root.fillColor
        }
    }

    Rectangle {
        x: (parent.width - width) * root.clamp(root.value)
        anchors.verticalCenter: parent.verticalCenter
        width: 10
        height: 10
        radius: width / 2
        color: root.fillColor
    }

    MouseArea {
        anchors.fill: parent

        onPressed: event => root.moved(root.clamp(event.x / width))
        onPositionChanged: event => {
            if (pressed)
                root.moved(root.clamp(event.x / width));
        }
    }
}
