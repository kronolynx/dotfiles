import QtQuick
import qs.theme
import qs.bar
import qs.services

// Screen brightness: scroll to adjust, click for a slider.
// Neutral cell colour so the coloured cells stay meaningful.
Cell {
    id: root

    icon: Icons.brightness
    label: `${Backlight.percent}%`
    showLabel: root.hovered || popup.showing
    cellColor: Config.dgrey
    fgColor: Config.white
    onScrolled: delta => Backlight.adjust(delta * 5)
    onClicked: popup.showing = !popup.showing

    Popup {
        id: popup

        anchorItem: root
        implicitWidth: 220
        // Sized from the content: a hardcoded height left the slider laid out
        // past the popup's bottom edge, drawn outside the window and therefore
        // impossible to click.
        implicitHeight: col.implicitHeight + Config.popupPad * 2

        Column {
            id: col

            width: parent.width
            spacing: 8

            Text {
                text: `Brightness ${Backlight.percent}%`
                color: Config.white
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize
                font.bold: true
            }

            Slider {
                width: parent.width
                // Taller than the track so it is easy to grab.
                implicitHeight: 18
                value: Backlight.value
                fillColor: Config.yellow
                onMoved: fraction => Backlight.setPercent(fraction * 100)
            }
        }
    }
}
