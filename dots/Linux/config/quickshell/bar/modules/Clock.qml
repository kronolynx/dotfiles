import Quickshell
import qs.theme
import qs.bar

// plugins/clock.sh: date '+%H:%M:%S', refreshed every second
Cell {
    icon: Icons.clock
    label: Qt.formatDateTime(clock.date, "HH:mm:ss")
    cellColor: Config.yellow

    SystemClock {
        id: clock

        precision: SystemClock.Seconds
    }
}
