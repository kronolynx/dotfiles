import QtQuick
import Quickshell
import Quickshell.Hyprland
import qs.theme
import qs.bar

// items/spaces.sh + plugins/front_app.sh, ported to Hyprland:
//  - a pill per workspace that has windows, or that is focused
//  - one app icon per distinct window class, with "*" appended when a class
//    appears more than once (that is what the asterisks on the Mac mean)
//  - the focused pill inverts to white
Row {
    id: root

    required property string screenName

    // Ties the icon bindings to DesktopEntries, which fills in asynchronously
    // entry by entry after startup.
    readonly property int entryCount: DesktopEntries.applications.values.length

    // Window classes per workspace, keyed by NAME rather than id: workspaces
    // that Quickshell creates while parsing `j/clients` can stay stuck at
    // id -1, but their name is always right.
    //
    // The class comes from the Wayland handle, not from lastIpcObject: only
    // windows that already existed when the shell started get an IPC object
    // (from the one-off `j/clients` fetch). Anything opened later is built from
    // the event socket alone, leaving lastIpcObject null forever -- which used
    // to feed an empty class straight into the generic fallback icon. appId is
    // maintained live by the foreign-toplevel protocol instead.
    readonly property var classesByWorkspace: {
        const map = {};
        for (const toplevel of Hyprland.toplevels.values) {
            const name = toplevel.workspace?.name;
            if (!name)
                continue;
            const cls = toplevel.wayland?.appId || toplevel.lastIpcObject?.class || "";
            if (!cls)
                continue;
            if (!map[name])
                map[name] = [];
            map[name].push(cls);
        }
        return map;
    }

    readonly property var workspaces: {
        const all = Hyprland.workspaces.values.filter(w => {
            if (!w.name || w.name.startsWith("special"))
                return false;
            if (!Config.workspacesPerMonitor)
                return true;
            // Keep workspaces whose monitor is not known yet, or this one's.
            return !w.monitor || w.monitor.name === root.screenName;
        });
        return all.sort((a, b) => root.numberOf(a.name) - root.numberOf(b.name));
    }

    function numberOf(name) {
        const n = parseInt(name);
        return isNaN(n) ? 1e6 : n;
    }

    // Distinct classes, in order, flagged when the same app appears twice.
    function appsOn(name) {
        const classes = root.classesByWorkspace[name] ?? [];
        const index = {};
        const out = [];
        for (const cls of classes) {
            if (index[cls] !== undefined)
                out[index[cls]].duplicate = true;
            else {
                index[cls] = out.length;
                out.push({
                    "cls": cls,
                    "duplicate": false
                });
            }
        }
        return out;
    }

    function iconFor(cls) {
        const entry = DesktopEntries.heuristicLookup(cls);
        return Quickshell.iconPath(entry?.icon ?? cls, "application-x-executable");
    }

    height: Config.cellHeight
    spacing: Config.cellGap

    Repeater {
        model: root.workspaces

        Cell {
            id: pill

            required property var modelData

            readonly property bool focused: Hyprland.focusedWorkspace?.name === modelData.name
            readonly property var apps: root.appsOn(modelData.name)

            // front_app.sh hides an empty workspace unless it is focused.
            readonly property bool shown: pill.apps.length > 0 || pill.focused

            visible: pill.shown
            width: pill.shown ? implicitWidth : 0

            label: modelData.name
            showLabel: true
            cellColor: pill.focused ? Config.white : Config.dgrey
            fgColor: pill.focused ? Config.black : Config.white
            // activate() rather than Hyprland.dispatch("workspace N"): a lua
            // config (hypr/hyprland.lua) makes Hyprland evaluate dispatch
            // strings as lua, so the classic form is rejected outright --
            // `hyprctl dispatch workspace 2` answers `')' expected near '2'`.
            // Hyprland.usingLua is no help, it reads false here even so.
            onClicked: modelData.activate()

            Row {
                height: parent.height
                spacing: 3
                rightPadding: pill.apps.length > 0 ? Config.iconPad : 0

                Repeater {
                    model: pill.apps

                    Row {
                        required property var modelData

                        height: parent.height
                        spacing: 0

                        Image {
                            anchors.verticalCenter: parent.verticalCenter
                            width: 15
                            height: 15
                            sourceSize.width: 32
                            sourceSize.height: 32
                            smooth: true
                            source: root.entryCount >= 0 ? root.iconFor(modelData.cls) : ""
                        }

                        Text {
                            anchors.verticalCenter: parent.verticalCenter
                            visible: modelData.duplicate
                            text: "*"
                            color: pill.fgColor
                            font.family: Config.labelFont
                            font.pixelSize: Config.labelSize
                            font.bold: true
                        }
                    }
                }
            }
        }
    }
}
