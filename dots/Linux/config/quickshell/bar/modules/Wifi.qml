import QtQuick
import QtQuick.Controls
import Quickshell
import Quickshell.Io
import Quickshell.Networking
import qs.theme
import qs.bar

// No sketchybar equivalent: wifi lives in the macOS menu bar on the Mac.
// Backed by Quickshell.Networking, so nm-applet is not needed.
Cell {
    id: root

    readonly property var device: {
        const devices = Networking.devices.values;
        return devices.find(d => d.type === DeviceType.Wifi) ?? null;
    }
    readonly property var networks: root.device?.networks?.values ?? []
    readonly property var activeNet: root.networks.find(n => n.connected) ?? null
    property string lastError: ""

    // One row per SSID: connected first, then saved networks, then the rest.
    //
    // Sorted by signal *bucket* rather than raw signal, and by name within a
    // bucket. Raw signal jitters constantly while scanning -- this list was
    // recomputed 56 times in 20 seconds -- and every reorder moves rows out
    // from under the pointer. Buckets change rarely, so rows hold still.
    readonly property var visibleNets: {
        const seen = {};
        const out = [];
        for (const net of root.networks) {
            if (!net.name || seen[net.name])
                continue;
            seen[net.name] = true;
            out.push(net);
        }
        return out.sort((a, b) => {
            if (a.connected !== b.connected)
                return a.connected ? -1 : 1;
            if (a.known !== b.known)
                return a.known ? -1 : 1;
            const bucket = Math.round(b.signalStrength * 4) - Math.round(a.signalStrength * 4);
            return bucket !== 0 ? bucket : a.name.localeCompare(b.name);
        });
    }

    function askPassword(net) {
        root.lastError = "";
        prompt.open(net.name, net.known);
    }

    icon: {
        if (!Networking.wifiEnabled)
            return Icons.wifiOff;
        if (!root.activeNet)
            return Icons.wifiAlert;
        return Icons.wifi(root.activeNet.signalStrength);
    }
    cellColor: Config.orange
    onClicked: popup.showing = !popup.showing

    // Scanning is off by default, which is why only the connected network
    // shows up until something asks for a scan.
    Binding {
        target: root.device
        property: "scannerEnabled"
        value: popup.showing || prompt.visible
        when: root.device !== null && root.device.type === DeviceType.Wifi
    }

    PasswordPrompt {
        id: prompt

        onCancelled: {
            close();
            root.lastError = "";
        }

        // The password goes in over stdin, so it never shows up in the process
        // list or in shell history.
        //
        // psk-flags 0 is the point of the modify: it tells NetworkManager to
        // store the key in the connection profile itself. Profiles created by
        // a desktop environment are often "agent-owned" (flags 1) instead,
        // meaning NM holds no key and asks a secret agent for it every time --
        // and this session runs no NM secret agent, so those connections fail
        // with NoSecrets. `nmcli device wifi connect` already stores the key
        // this way, so networks joined here are remembered from then on.
        onSubmitted: password => {
            joinProc.command = ["sh", "-c", "read -r psk\n" + "if [ \"$2\" = known ]; then\n" + "  nmcli connection modify \"$1\" 802-11-wireless-security.psk \"$psk\" 802-11-wireless-security.psk-flags 0 &&\n" + "  nmcli connection up \"$1\"\n" + "else\n" + "  nmcli device wifi connect \"$1\" password \"$psk\"\n" + "fi", "sh", prompt.ssid, prompt.known ? "known" : "new"];
            joinProc.running = true;
            joinProc.write(`${password}\n`);
            prompt.close();
        }
    }

    Process {
        id: joinProc

        stdinEnabled: true

        stderr: StdioCollector {
            onStreamFinished: {
                if (text.trim() !== "")
                    root.lastError = text.trim().split("\n").pop();
            }
        }

        onExited: exitCode => {
            if (exitCode === 0)
                root.lastError = "";
        }
    }

    Popup {
        id: popup

        anchorItem: root
        implicitWidth: 320
        implicitHeight: col.implicitHeight + Config.popupPad * 2

        onShowingChanged: {
            if (!showing)
                root.lastError = "";
        }

        Column {
            id: col

            width: parent.width
            spacing: 6

            Row {
                width: parent.width
                spacing: 8

                Text {
                    width: parent.width - toggle.width - 8
                    text: root.activeNet ? root.activeNet.name : (Networking.wifiEnabled ? "Not connected" : "Wi-Fi off")
                    elide: Text.ElideRight
                    color: Config.white
                    font.family: Config.labelFont
                    font.pixelSize: Config.labelSize
                    font.bold: true
                }

                Rectangle {
                    id: toggle

                    width: 40
                    height: 18
                    radius: 9
                    color: Networking.wifiEnabled ? Config.orange : Config.dgrey

                    Rectangle {
                        x: Networking.wifiEnabled ? parent.width - width - 2 : 2
                        y: 2
                        width: 14
                        height: 14
                        radius: 7
                        color: Config.white

                        Behavior on x {
                            NumberAnimation {
                                duration: 120
                            }
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: Networking.wifiEnabled = !Networking.wifiEnabled
                    }
                }
            }

            Text {
                width: parent.width
                visible: root.lastError !== ""
                text: root.lastError
                wrapMode: Text.Wrap
                color: Config.red
                font.family: Config.labelFont
                font.pixelSize: Config.labelSize - 1
            }

            Rectangle {
                width: parent.width
                height: 1
                color: Config.dgrey
            }

            // Scrollable: there are ~24 networks in range here, far more than
            // fits. Without this the extra rows were laid out past the popup's
            // edge, drawn nowhere and impossible to click.
            Flickable {
                width: parent.width
                height: Math.min(rows.implicitHeight, 264)
                contentWidth: width
                contentHeight: rows.implicitHeight
                clip: true
                boundsBehavior: Flickable.StopAtBounds

                ScrollBar.vertical: ScrollBar {
                    policy: rows.implicitHeight > 264 ? ScrollBar.AlwaysOn : ScrollBar.AlwaysOff
                }

                Column {
                    id: rows

                    width: parent.width
                    spacing: 1

                    Repeater {
                        // ScriptModel diffs the array instead of replacing it,
                        // so rows keep their identity across scan updates. With
                        // a plain array the Repeater tore down and rebuilt every
                        // delegate ~3x a second, and a click could not survive
                        // from press to release.
                        model: ScriptModel {
                            values: root.visibleNets
                        }

                        Rectangle {
                            id: netRow

                            required property var modelData

                            width: rows.width
                            height: 28
                            radius: Config.radius
                            color: hover.containsMouse ? Config.rowHover : "transparent"

                            Connections {
                                target: netRow.modelData

                                function onConnectionFailed(reason) {
                                    // A saved network that fails for want of a
                                    // key has no agent to ask, so ask here.
                                    if (reason === ConnectionFailReason.NoSecrets) {
                                        root.lastError = `${netRow.modelData.name} needs its password`;
                                        root.askPassword(netRow.modelData);
                                    } else {
                                        root.lastError = `${netRow.modelData.name}: ${ConnectionFailReason.toString(reason)}`;
                                    }
                                }
                            }

                            Row {
                                anchors.verticalCenter: parent.verticalCenter
                                anchors.left: parent.left
                                anchors.right: parent.right
                                anchors.leftMargin: 6
                                anchors.rightMargin: 6
                                spacing: 8

                                Text {
                                    width: 18
                                    anchors.verticalCenter: parent.verticalCenter
                                    text: Icons.wifi(netRow.modelData.signalStrength)
                                    color: netRow.modelData.connected ? Config.orange : Config.grey
                                    font.family: Config.iconFont
                                    font.pixelSize: Config.iconSize
                                }

                                Text {
                                    width: parent.width - 18 - 8 - 14 - 16
                                    anchors.verticalCenter: parent.verticalCenter
                                    text: netRow.modelData.name
                                    elide: Text.ElideRight
                                    color: netRow.modelData.connected || netRow.modelData.known ? Config.white : Config.grey
                                    font.family: Config.labelFont
                                    font.pixelSize: Config.labelSize
                                    font.bold: netRow.modelData.connected
                                }

                                Text {
                                    width: 14
                                    anchors.verticalCenter: parent.verticalCenter
                                    visible: netRow.modelData.stateChanging
                                    text: "…"
                                    color: Config.orange
                                    font.family: Config.labelFont
                                    font.pixelSize: Config.labelSize
                                }

                                Text {
                                    width: 14
                                    anchors.verticalCenter: parent.verticalCenter
                                    visible: netRow.modelData.security !== WifiSecurityType.Open
                                    text: Icons.lock
                                    color: Config.dgrey
                                    font.family: Config.iconFont
                                    font.pixelSize: Config.iconSize - 3
                                }
                            }

                            MouseArea {
                                id: hover

                                anchors.fill: parent
                                hoverEnabled: true
                                acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton

                                onClicked: event => {
                                    const net = netRow.modelData;
                                    root.lastError = "";
                                    // Middle click always asks for the password,
                                    // for retyping a key that has changed.
                                    if (event.button === Qt.MiddleButton) {
                                        root.askPassword(net);
                                        return;
                                    }
                                    if (event.button === Qt.RightButton) {
                                        if (net.known)
                                            net.forget();
                                        return;
                                    }
                                    if (net.connected)
                                        net.disconnect();
                                    else if (net.known || net.security === WifiSecurityType.Open)
                                        net.connect();
                                    else
                                        root.askPassword(net);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
