import QtQuick
import Quickshell.Services.Pipewire
import qs.theme
import qs.bar

// items/volume.sh + plugins/volume.sh: icon by bucket, and a slider that slides
// out on hover (sketchybar animated slider.width 0 -> 100) and back when the
// pointer leaves.
Cell {
    id: root

    readonly property var sink: Pipewire.defaultAudioSink
    readonly property var audio: sink?.audio ?? null
    readonly property int pct: Math.round((audio?.volume ?? 0) * 100)
    readonly property bool muted: audio?.muted ?? false
    readonly property bool expanded: root.hovered

    function setVolume(v) {
        if (root.audio)
            root.audio.volume = Math.max(0, Math.min(1, v));
    }

    icon: Icons.volume(root.pct, root.muted)
    iconWidth: 30 // volume.sh icon.width=30
    cellColor: Config.blue
    onClicked: {
        if (root.audio)
            root.audio.muted = !root.audio.muted;
    }
    onScrolled: delta => root.setVolume((root.audio?.volume ?? 0) + delta * 0.05)

    // Without a tracker the sink's audio properties are never bound.
    PwObjectTracker {
        objects: [Pipewire.defaultAudioSink]
    }

    Item {
        implicitWidth: root.expanded ? 104 : 0
        height: parent.height
        clip: true

        Behavior on implicitWidth {
            NumberAnimation {
                duration: 200
                easing.type: Easing.OutCubic
            }
        }

        Slider {
            anchors.verticalCenter: parent.verticalCenter
            x: 0
            width: 96
            opacity: root.expanded ? 1 : 0
            value: root.muted ? 0 : root.audio?.volume ?? 0
            trackColor: Qt.darker(Config.blue, 1.6)
            onMoved: v => {
                if (root.audio) {
                    root.audio.muted = false;
                    root.setVolume(v);
                }
            }

            Behavior on opacity {
                NumberAnimation {
                    duration: 150
                }
            }
        }
    }
}
