import Quickshell
import QtQuick
import Quickshell.Services.UPower
import qs.theme
import qs.bar

// plugins/battery.sh: icon by bucket, icon turns orange under 30 and red under
// 10, the label is hidden at 100%, and charging swaps the glyph out.
Cell {
    id: root

    readonly property var bat: UPower.displayDevice
    // Quickshell reports percentage as a 0-1 fraction, not 0-100.
    readonly property int pct: Math.round((bat?.percentage ?? 0) * 100)
    readonly property bool charging: bat?.state === UPowerDeviceState.Charging
        || bat?.state === UPowerDeviceState.PendingCharge
    readonly property bool full: bat?.state === UPowerDeviceState.FullyCharged

    function humanTime(seconds) {
        if (!seconds || seconds <= 0)
            return "";
        const h = Math.floor(seconds / 3600);
        const m = Math.round((seconds % 3600) / 60);
        return h > 0 ? `${h}h ${m}m` : `${m}m`;
    }

    icon: Icons.battery(root.pct, root.charging)
    iconWidth: 30 // battery.sh icon.width=30
    label: `${root.pct}%`
    showLabel: root.pct < 100
    cellColor: Config.magenta
    iconColor: root.pct < 10 ? Config.red : (root.pct < 30 ? Config.orange : Config.black)
    onClicked: popup.showing = !popup.showing

    Popup {
        id: popup

        anchorItem: root
        implicitWidth: 190
        implicitHeight: col.implicitHeight + Config.popupPad * 2

        Column {
            id: col

            width: parent.width
            spacing: 4

            Text {
                text: root.charging ? "Charging" : (root.full ? "Fully charged" : "On battery")
                color: Config.white
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize
                font.bold: true
            }

            Text {
                readonly property string remaining: root.charging
                    ? root.humanTime(root.bat?.timeToFull)
                    : root.humanTime(root.bat?.timeToEmpty)

                visible: remaining !== ""
                text: root.charging ? `${remaining} until full` : `${remaining} remaining`
                color: Config.grey
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize
            }

            Text {
                visible: (root.bat?.healthSupported ?? false) && (root.bat?.healthPercentage ?? 0) > 0
                text: `Health ${Math.round(root.bat?.healthPercentage ?? 0)}%`
                color: Config.grey
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize
            }

            Text {
                visible: (root.bat?.changeRate ?? 0) > 0
                text: `${(root.bat?.changeRate ?? 0).toFixed(1)} W`
                color: Config.grey
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize
            }
        }
    }
}
