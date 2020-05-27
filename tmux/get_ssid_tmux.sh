#!/bin/sh

airport_path="/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport"

if air_info=($(eval "$airport_path" -I | grep -E "^ *(agrCtlRSSI|state|SSID):" | awk '{print $2}')) ; then

  rssi=${air_info[0]}
  state=${air_info[1]}
  ssid=${air_info[2]}

  case "$state" in
    "running" )
      signal=""
      rssi_=$(expr 5 - ${rssi} / -20)
      airport_="${ssid} "
  esac
  echo "#[default]${airport_}| "
fi
