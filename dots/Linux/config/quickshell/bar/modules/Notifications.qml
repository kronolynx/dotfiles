import qs.theme
import qs.bar
import qs.services

// items/notifications.sh: cyan cell, bell glyph, white when something is
// waiting. Click toggles the notification centre; right click toggles DND.
Cell {
    icon: Swaync.dnd ? Icons.bellOff : (Swaync.count > 0 ? Icons.bellUnread : Icons.bell)
    iconColor: Swaync.count > 0 ? Config.white : Config.black
    label: Swaync.count > 0 ? `${Swaync.count}` : ""
    cellColor: Config.cyan
    onClicked: Swaync.toggle()
    onRightClicked: Swaync.toggleDnd()
}
