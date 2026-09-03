import QtQuick
import qs.theme

// A floating group of cells. This is sketchybar's "bracket": a rounded black
// slab with a 3px border around cells that are themselves 23px tall.
Rectangle {
    id: root

    default property alias content: row.data
    readonly property alias row: row

    implicitWidth: row.implicitWidth + Config.islandPad * 2
    implicitHeight: Config.cellHeight + Config.islandPad * 2
    radius: Config.radius + 1
    color: Config.black

    Row {
        id: row

        x: Config.islandPad
        y: Config.islandPad
        height: Config.cellHeight
        spacing: Config.cellGap
    }
}
