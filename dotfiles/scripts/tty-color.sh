#!/bin/sh
[ "${TERM:-none}" = "linux" ] && \
    printf '%b' '\e]P00C0201
                 \e]P10E0A4B
                 \e]P2BD5758
                 \e]P3B48D62
                 \e]P4C39D60
                 \e]P5F7877E
                 \e]P6DE6C82
                 \e]P7ebb2b7
                 \e]P8a47c80
                 \e]P90E0A4B
                 \e]PABD5758
                 \e]PBB48D62
                 \e]PCC39D60
                 \e]PDF7877E
                 \e]PEDE6C82
                 \e]PFebb2b7
                 \ec'
