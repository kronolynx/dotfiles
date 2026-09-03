import Quickshell
import QtQuick
import QtQuick.Controls
import qs.theme
import qs.bar

// plugins/calendar.sh: date '+%a %d. %b', click opened Calendar.app.
// The locale is pinned to en_US on purpose: this machine's locale is Dutch,
// which would render "do 03 sep" instead of the "Thu 03. Sep" the Mac shows.
Cell {
    id: root

    readonly property var enUS: Qt.locale("en_US")

    // Months away from the current one. Reset whenever the popup closes, so it
    // always opens on today.
    property int monthOffset: 0

    readonly property date shownMonth: new Date(clock.date.getFullYear(), clock.date.getMonth() + root.monthOffset, 1)

    icon: Icons.calendar
    label: clock.date.toLocaleDateString(root.enUS, "ddd dd. MMM")
    cellColor: Config.green
    onClicked: popup.showing = !popup.showing

    SystemClock {
        id: clock

        precision: SystemClock.Minutes
    }

    Popup {
        id: popup

        anchorItem: root
        implicitWidth: col.implicitWidth + Config.popupPad * 2
        implicitHeight: col.implicitHeight + Config.popupPad * 2

        onShowingChanged: {
            if (!showing)
                root.monthOffset = 0;
        }

        Column {
            id: col

            spacing: 2

            // Header: ‹ month year › -- click the title to jump back to today.
            // Width comes from the grid, not from col: col's own implicitWidth
            // is derived from its children, so measuring it here would loop.
            Item {
                width: grid.implicitWidth
                height: 24

                Text {
                    id: prevMonth

                    anchors.left: parent.left
                    anchors.verticalCenter: parent.verticalCenter
                    width: 24
                    horizontalAlignment: Text.AlignHCenter
                    text: Icons.chevronLeft
                    color: prevArea.containsMouse ? Config.green : Config.grey
                    font.family: Config.iconFont
                    font.pixelSize: Config.iconSize

                    MouseArea {
                        id: prevArea

                        anchors.fill: parent
                        hoverEnabled: true
                        onClicked: root.monthOffset -= 1
                    }
                }

                Text {
                    anchors.centerIn: parent
                    text: root.shownMonth.toLocaleDateString(root.enUS, "MMMM yyyy")
                    color: root.monthOffset === 0 ? Config.white : Config.green
                    font.family: Config.labelFont
                    font.pixelSize: Config.labelSize
                    font.bold: true

                    MouseArea {
                        anchors.fill: parent
                        anchors.margins: -4
                        hoverEnabled: true
                        cursorShape: root.monthOffset === 0 ? Qt.ArrowCursor : Qt.PointingHandCursor
                        onClicked: root.monthOffset = 0
                    }
                }

                Text {
                    id: nextMonth

                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter
                    width: 24
                    horizontalAlignment: Text.AlignHCenter
                    text: Icons.chevronRight
                    color: nextArea.containsMouse ? Config.green : Config.grey
                    font.family: Config.iconFont
                    font.pixelSize: Config.iconSize

                    MouseArea {
                        id: nextArea

                        anchors.fill: parent
                        hoverEnabled: true
                        onClicked: root.monthOffset += 1
                    }
                }
            }

            DayOfWeekRow {
                locale: root.enUS
                spacing: 2
                padding: 0

                delegate: Text {
                    required property string shortName

                    width: 30
                    horizontalAlignment: Text.AlignHCenter
                    text: shortName
                    color: Config.grey
                    font.family: Config.labelFont
                    font.pixelSize: Config.labelSize - 2
                    font.bold: true
                }
            }

            MonthGrid {
                id: grid

                month: root.shownMonth.getMonth()
                year: root.shownMonth.getFullYear()
                locale: root.enUS
                spacing: 2
                padding: 0

                delegate: Rectangle {
                    required property var model

                    implicitWidth: 30
                    implicitHeight: 26
                    radius: Config.radius
                    // MonthGrid marks the real today, so this only lights up
                    // when the current month is the one on screen.
                    color: model.today ? Config.green : "transparent"

                    Text {
                        anchors.centerIn: parent
                        text: model.day
                        color: model.today ? Config.black : (model.month === grid.month ? Config.white : Config.dgrey)
                        font.family: Config.labelFont
                        font.pixelSize: Config.labelSize
                        font.bold: model.today
                    }
                }
            }

        }

        // Scrolling anywhere over the calendar changes month too. A sibling of
        // the Column rather than a child, because positioners lay their
        // children out and reject anchors; NoButton so it never swallows the
        // clicks meant for the header.
        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.NoButton
            onWheel: event => {
                root.monthOffset += event.angleDelta.y > 0 ? -1 : 1;
            }
        }
    }
}
