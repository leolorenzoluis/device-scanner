module IML.DeviceAggregatorDaemon.TestFixtures

open Fable.Import
open IML.Types.MessageTypes

let toSerialised =
  JS.JSON.parse
    >> JS.JSON.stringify

let updateString = toSerialised """
{
  "zed": {
    "16895995351780274": {
      "name": "testPool",
      "guid": "16895995351780274",
      "health": "ONLINE",
      "hostname": "lotus-32vm5",
      "hostid": 532248858,
      "state": "ACTIVE",
      "readonly": false,
      "size": 10670309376,
      "vdev": {
        "Root": {
          "children": [
            {
              "Disk": {
                "guid": "0x5C01CAA35E9CC2A0",
                "state": "ONLINE",
                "path": "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_disk4-part1",
                "dev_id": "scsi-0QEMU_QEMU_HARDDISK_disk4-part1",
                "phys_path": "virtio-pci-0000:00:05.0-scsi-0:0:0:3",
                "whole_disk": true,
                "is_log": false
              }
            }
          ],
          "spares": [],
          "cache": []
        }
      },
      "props": [],
      "datasets": []
    }
  },
  "blockDevices": {
    "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:0/block/sda": {
      "major": "8",
      "minor": "0",
      "paths": [
        "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_disk1",
        "/dev/disk/by-path/virtio-pci-0000:00:05.0-scsi-0:0:0:0",
        "/dev/sda"
      ],
      "devName": "/dev/sda",
      "devPath": "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:0/block/sda",
      "devType": "disk",
      "idVendor": "QEMU",
      "idModel": "QEMU_HARDDISK",
      "idSerial": "0QEMU_QEMU_HARDDISK_disk1",
      "idFsType": "linux_raid_member",
      "idFsUsage": "raid",
      "idFsUuid": "4469795b-65e0-ea35-1901-01ca93fe194f",
      "idPartEntryNumber": null,
      "size": "20971520",
      "scsi80": "SQEMU    QEMU HARDDISK   disk1",
      "scsi83": "0QEMU    QEMU HARDDISK   disk1",
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:1/block/sde": {
      "major": "8",
      "minor": "64",
      "paths": [
        "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_disk2",
        "/dev/disk/by-path/virtio-pci-0000:00:05.0-scsi-0:0:0:1",
        "/dev/sde"
      ],
      "devName": "/dev/sde",
      "devPath": "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:1/block/sde",
      "devType": "disk",
      "idVendor": "QEMU",
      "idModel": "QEMU_HARDDISK",
      "idSerial": "0QEMU_QEMU_HARDDISK_disk2",
      "idFsType": null,
      "idFsUsage": null,
      "idFsUuid": null,
      "idPartEntryNumber": null,
      "size": "20971520",
      "scsi80": "SQEMU    QEMU HARDDISK   disk2",
      "scsi83": "0QEMU    QEMU HARDDISK   disk2",
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:1/block/sde/sde1": {
      "major": "8",
      "minor": "65",
      "paths": [
        "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_disk2-part1",
        "/dev/disk/by-path/virtio-pci-0000:00:05.0-scsi-0:0:0:1-part1",
        "/dev/disk/by-uuid/8ca30050-4d6b-41e7-99fc-1cee2991845c",
        "/dev/sde1"
      ],
      "devName": "/dev/sde1",
      "devPath": "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:1/block/sde/sde1",
      "devType": "partition",
      "idVendor": "QEMU",
      "idModel": "QEMU_HARDDISK",
      "idSerial": "0QEMU_QEMU_HARDDISK_disk2",
      "idFsType": "ext4",
      "idFsUsage": "filesystem",
      "idFsUuid": "8ca30050-4d6b-41e7-99fc-1cee2991845c",
      "idPartEntryNumber": 1,
      "size": "20969472",
      "scsi80": "SQEMU    QEMU HARDDISK   disk2",
      "scsi83": "0QEMU    QEMU HARDDISK   disk2",
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:2/block/sdd": {
      "major": "8",
      "minor": "48",
      "paths": [
        "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_disk3",
        "/dev/disk/by-path/virtio-pci-0000:00:05.0-scsi-0:0:0:2",
        "/dev/sdd"
      ],
      "devName": "/dev/sdd",
      "devPath": "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:2/block/sdd",
      "devType": "disk",
      "idVendor": "QEMU",
      "idModel": "QEMU_HARDDISK",
      "idSerial": "0QEMU_QEMU_HARDDISK_disk3",
      "idFsType": "linux_raid_member",
      "idFsUsage": "raid",
      "idFsUuid": "4469795b-65e0-ea35-1901-01ca93fe194f",
      "idPartEntryNumber": null,
      "size": "20971520",
      "scsi80": "SQEMU    QEMU HARDDISK   disk3",
      "scsi83": "0QEMU    QEMU HARDDISK   disk3",
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:3/block/sdc": {
      "major": "8",
      "minor": "32",
      "paths": [
        "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_disk4",
        "/dev/disk/by-path/virtio-pci-0000:00:05.0-scsi-0:0:0:3",
        "/dev/sdc"
      ],
      "devName": "/dev/sdc",
      "devPath": "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:3/block/sdc",
      "devType": "disk",
      "idVendor": "QEMU",
      "idModel": "QEMU_HARDDISK",
      "idSerial": "0QEMU_QEMU_HARDDISK_disk4",
      "idFsType": null,
      "idFsUsage": null,
      "idFsUuid": null,
      "idPartEntryNumber": null,
      "size": "20971520",
      "scsi80": "SQEMU    QEMU HARDDISK   disk4",
      "scsi83": "0QEMU    QEMU HARDDISK   disk4",
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:3/block/sdc/sdc1": {
      "major": "8",
      "minor": "33",
      "paths": [
        "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_disk4-part1",
        "/dev/disk/by-label/testPool",
        "/dev/disk/by-partlabel/zfs-fddd2aca10d3f23e",
        "/dev/disk/by-partuuid/4960d69b-e9f3-5e47-ac22-b09b348fdd27",
        "/dev/disk/by-path/virtio-pci-0000:00:05.0-scsi-0:0:0:3-part1",
        "/dev/disk/by-uuid/16895995351780274",
        "/dev/sdc1"
      ],
      "devName": "/dev/sdc1",
      "devPath": "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:3/block/sdc/sdc1",
      "devType": "partition",
      "idVendor": "QEMU",
      "idModel": "QEMU_HARDDISK",
      "idSerial": "0QEMU_QEMU_HARDDISK_disk4",
      "idFsType": "zfs_member",
      "idFsUsage": "filesystem",
      "idFsUuid": "16895995351780274",
      "idPartEntryNumber": 1,
      "size": "20951040",
      "scsi80": "SQEMU    QEMU HARDDISK   disk4",
      "scsi83": "0QEMU    QEMU HARDDISK   disk4",
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:3/block/sdc/sdc9": {
      "major": "8",
      "minor": "41",
      "paths": [
        "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_disk4-part9",
        "/dev/disk/by-partuuid/aad906a6-ff5e-6d40-8c2d-9ce53a35b330",
        "/dev/disk/by-path/virtio-pci-0000:00:05.0-scsi-0:0:0:3-part9",
        "/dev/sdc9"
      ],
      "devName": "/dev/sdc9",
      "devPath": "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:3/block/sdc/sdc9",
      "devType": "partition",
      "idVendor": "QEMU",
      "idModel": "QEMU_HARDDISK",
      "idSerial": "0QEMU_QEMU_HARDDISK_disk4",
      "idFsType": null,
      "idFsUsage": null,
      "idFsUuid": null,
      "idPartEntryNumber": 9,
      "size": "16384",
      "scsi80": "SQEMU    QEMU HARDDISK   disk4",
      "scsi83": "0QEMU    QEMU HARDDISK   disk4",
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:4/block/sdb": {
      "major": "8",
      "minor": "16",
      "paths": [
        "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_disk5",
        "/dev/disk/by-path/virtio-pci-0000:00:05.0-scsi-0:0:0:4",
        "/dev/sdb"
      ],
      "devName": "/dev/sdb",
      "devPath": "/devices/pci0000:00/0000:00:05.0/virtio1/host2/target2:0:0/2:0:0:4/block/sdb",
      "devType": "disk",
      "idVendor": "QEMU",
      "idModel": "QEMU_HARDDISK",
      "idSerial": "0QEMU_QEMU_HARDDISK_disk5",
      "idFsType": "linux_raid_member",
      "idFsUsage": "raid",
      "idFsUuid": "4681a769-b5e6-62ad-c622-d1d4db91f5c4",
      "idPartEntryNumber": null,
      "size": "20971520",
      "scsi80": "SQEMU    QEMU HARDDISK   disk5",
      "scsi83": "0QEMU    QEMU HARDDISK   disk5",
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:06.0/virtio2/block/vda": {
      "major": "252",
      "minor": "0",
      "paths": [
        "/dev/disk/by-id/virtio-mds1-root",
        "/dev/disk/by-path/virtio-pci-0000:00:06.0",
        "/dev/vda"
      ],
      "devName": "/dev/vda",
      "devPath": "/devices/pci0000:00/0000:00:06.0/virtio2/block/vda",
      "devType": "disk",
      "idVendor": null,
      "idModel": null,
      "idSerial": "mds1-root",
      "idFsType": null,
      "idFsUsage": null,
      "idFsUuid": null,
      "idPartEntryNumber": null,
      "size": "41943040",
      "scsi80": null,
      "scsi83": null,
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:06.0/virtio2/block/vda/vda1": {
      "major": "252",
      "minor": "1",
      "paths": [
        "/dev/disk/by-id/virtio-mds1-root-part1",
        "/dev/disk/by-path/virtio-pci-0000:00:06.0-part1",
        "/dev/disk/by-uuid/39d14274-c014-4f4e-9fb5-007e0370a588",
        "/dev/vda1"
      ],
      "devName": "/dev/vda1",
      "devPath": "/devices/pci0000:00/0000:00:06.0/virtio2/block/vda/vda1",
      "devType": "partition",
      "idVendor": null,
      "idModel": null,
      "idSerial": "mds1-root",
      "idFsType": "ext3",
      "idFsUsage": "filesystem",
      "idFsUuid": "39d14274-c014-4f4e-9fb5-007e0370a588",
      "idPartEntryNumber": 1,
      "size": "1024000",
      "scsi80": null,
      "scsi83": null,
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/pci0000:00/0000:00:06.0/virtio2/block/vda/vda2": {
      "major": "252",
      "minor": "2",
      "paths": [
        "/dev/disk/by-id/lvm-pv-uuid-8pH56h-GILE-Hp6F-RofI-HKKx-Tf97-93mjX5",
        "/dev/disk/by-id/virtio-mds1-root-part2",
        "/dev/disk/by-path/virtio-pci-0000:00:06.0-part2",
        "/dev/vda2"
      ],
      "devName": "/dev/vda2",
      "devPath": "/devices/pci0000:00/0000:00:06.0/virtio2/block/vda/vda2",
      "devType": "partition",
      "idVendor": null,
      "idModel": null,
      "idSerial": "mds1-root",
      "idFsType": "LVM2_member",
      "idFsUsage": "raid",
      "idFsUuid": "8pH56h-GILE-Hp6F-RofI-HKKx-Tf97-93mjX5",
      "idPartEntryNumber": 2,
      "size": "40916992",
      "scsi80": null,
      "scsi83": null,
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": null
    },
    "/devices/virtual/block/dm-0": {
      "major": "253",
      "minor": "0",
      "paths": [
        "/dev/disk/by-id/dm-name-vg_00-lv_root",
        "/dev/disk/by-id/dm-uuid-LVM-pV8TgNKMJVNrolJgMhVwg4CAeFFAIMC83Ch5TjlWtPw1BCu2ytrGIjlgzeo7oEtu",
        "/dev/disk/by-uuid/c1fdb5e9-c601-4cd4-b68b-8a89ba88775c",
        "/dev/mapper/vg_00-lv_root",
        "/dev/vg_00/lv_root",
        "/dev/dm-0"
      ],
      "devName": "/dev/dm-0",
      "devPath": "/devices/virtual/block/dm-0",
      "devType": "disk",
      "idVendor": null,
      "idModel": null,
      "idSerial": null,
      "idFsType": "ext4",
      "idFsUsage": "filesystem",
      "idFsUuid": "c1fdb5e9-c601-4cd4-b68b-8a89ba88775c",
      "idPartEntryNumber": null,
      "size": "36806656",
      "scsi80": null,
      "scsi83": null,
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [
        "252:2"
      ],
      "dmVgSize": "20946354176B",
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": "lv_root",
      "dmVgName": "vg_00",
      "dmUuid": "LVM-pV8TgNKMJVNrolJgMhVwg4CAeFFAIMC83Ch5TjlWtPw1BCu2ytrGIjlgzeo7oEtu",
      "mdUuid": null
    },
    "/devices/virtual/block/dm-1": {
      "major": "253",
      "minor": "1",
      "paths": [
        "/dev/disk/by-id/dm-name-vg_00-lv_swap",
        "/dev/disk/by-id/dm-uuid-LVM-pV8TgNKMJVNrolJgMhVwg4CAeFFAIMC83IU1hvimWWlvmd5xQddtMIqRtjwOuKTz",
        "/dev/disk/by-uuid/3121d9ed-fddd-4fd0-8e73-75ec48c36498",
        "/dev/mapper/vg_00-lv_swap",
        "/dev/vg_00/lv_swap",
        "/dev/dm-1"
      ],
      "devName": "/dev/dm-1",
      "devPath": "/devices/virtual/block/dm-1",
      "devType": "disk",
      "idVendor": null,
      "idModel": null,
      "idSerial": null,
      "idFsType": "swap",
      "idFsUsage": "other",
      "idFsUuid": "3121d9ed-fddd-4fd0-8e73-75ec48c36498",
      "idPartEntryNumber": null,
      "size": "4104192",
      "scsi80": null,
      "scsi83": null,
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [
        "252:2"
      ],
      "dmVgSize": "20946354176B",
      "mdDevices": [],
      "dmMultipathDevicePath": null,
      "dmLvName": "lv_swap",
      "dmVgName": "vg_00",
      "dmUuid": "LVM-pV8TgNKMJVNrolJgMhVwg4CAeFFAIMC83IU1hvimWWlvmd5xQddtMIqRtjwOuKTz",
      "mdUuid": null
    },
    "/devices/virtual/block/md126": {
      "major": "9",
      "minor": "126",
      "paths": [
        "/dev/disk/by-id/md-name-lotus-32vm5:0",
        "/dev/disk/by-id/md-uuid-4681a769:b5e662ad:c622d1d4:db91f5c4",
        "/dev/disk/by-label/f4-MDT0000",
        "/dev/disk/by-uuid/1b7b4e93-3279-4158-815c-63929a4a244e",
        "/dev/md/lotus-32vm5:0",
        "/dev/md126"
      ],
      "devName": "/dev/md126",
      "devPath": "/devices/virtual/block/md126",
      "devType": "disk",
      "idVendor": null,
      "idModel": null,
      "idSerial": null,
      "idFsType": "ext4",
      "idFsUsage": "filesystem",
      "idFsUuid": "1b7b4e93-3279-4158-815c-63929a4a244e",
      "idPartEntryNumber": null,
      "size": "20955136",
      "scsi80": null,
      "scsi83": null,
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [
        "/dev/sdb"
      ],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": "4681a769:b5e662ad:c622d1d4:db91f5c4"
    },
    "/devices/virtual/block/md127": {
      "major": "9",
      "minor": "127",
      "paths": [
        "/dev/disk/by-id/md-name-lotus-32vm6:0",
        "/dev/disk/by-id/md-uuid-4469795b:65e0ea35:190101ca:93fe194f",
        "/dev/disk/by-label/MGS",
        "/dev/disk/by-uuid/57bdf44a-fc02-4301-aef4-94f3dba26189",
        "/dev/md/lotus-32vm6:0",
        "/dev/md127"
      ],
      "devName": "/dev/md127",
      "devPath": "/devices/virtual/block/md127",
      "devType": "disk",
      "idVendor": null,
      "idModel": null,
      "idSerial": null,
      "idFsType": "ext4",
      "idFsUsage": "filesystem",
      "idFsUuid": "57bdf44a-fc02-4301-aef4-94f3dba26189",
      "idPartEntryNumber": null,
      "size": "41910272",
      "scsi80": null,
      "scsi83": null,
      "isReadOnly": false,
      "isBiosBoot": false,
      "dmSlaveMms": [],
      "dmVgSize": null,
      "mdDevices": [
        "/dev/sda",
        "/dev/sdd"
      ],
      "dmMultipathDevicePath": null,
      "dmLvName": null,
      "dmVgName": null,
      "dmUuid": null,
      "mdUuid": "4469795b:65e0ea35:190101ca:93fe194f"
    }
  },
  "localMounts": [
    {
      "target": "/sys/fs/cgroup/hugetlb",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,hugetlb"
    },
    {
      "target": "/run",
      "source": "tmpfs",
      "fstype": "tmpfs",
      "opts": "rw,nosuid,nodev,mode=755"
    },
    {
      "target": "/dev/pts",
      "source": "devpts",
      "fstype": "devpts",
      "opts": "rw,nosuid,noexec,relatime,gid=5,mode=620,ptmxmode=000"
    },
    {
      "target": "/sys/fs/cgroup/cpuset",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,cpuset"
    },
    {
      "target": "/sys/fs/cgroup/cpu,cpuacct",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,cpuacct,cpu"
    },
    {
      "target": "/net",
      "source": "-hosts",
      "fstype": "autofs",
      "opts": "rw,relatime,fd=13,pgrp=1188,timeout=300,minproto=5,maxproto=5,indirect,pipe_ino=19952"
    },
    {
      "target": "/sys/fs/pstore",
      "source": "pstore",
      "fstype": "pstore",
      "opts": "rw,nosuid,nodev,noexec,relatime"
    },
    {
      "target": "/sys/fs/cgroup/pids",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,pids"
    },
    {
      "target": "/home",
      "source": "auto.home",
      "fstype": "autofs",
      "opts": "rw,relatime,fd=25,pgrp=1188,timeout=300,minproto=5,maxproto=5,indirect,pipe_ino=19969"
    },
    {
      "target": "/proc",
      "source": "proc",
      "fstype": "proc",
      "opts": "rw,nosuid,nodev,noexec,relatime"
    },
    {
      "target": "/sys",
      "source": "sysfs",
      "fstype": "sysfs",
      "opts": "rw,nosuid,nodev,noexec,relatime"
    },
    {
      "target": "/sys/fs/cgroup/blkio",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,blkio"
    },
    {
      "target": "/proc/sys/fs/binfmt_misc",
      "source": "binfmt_misc",
      "fstype": "binfmt_misc",
      "opts": "rw,relatime"
    },
    {
      "target": "/sys/fs/cgroup/devices",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,devices"
    },
    {
      "target": "/scratch",
      "source": "auto.direct",
      "fstype": "autofs",
      "opts": "rw,relatime,fd=19,pgrp=1188,timeout=300,minproto=5,maxproto=5,direct,pipe_ino=19956"
    },
    {
      "target": "/sys/fs/cgroup/net_cls,net_prio",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,net_prio,net_cls"
    },
    {
      "target": "/sys/fs/cgroup/systemd",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,xattr,release_agent=/usr/lib/systemd/systemd-cgroups-agent,name=systemd"
    },
    {
      "target": "/proc/sys/fs/binfmt_misc",
      "source": "systemd-1",
      "fstype": "autofs",
      "opts": "rw,relatime,fd=37,pgrp=1,timeout=0,minproto=5,maxproto=5,direct,pipe_ino=10505"
    },
    {
      "target": "/dev/shm",
      "source": "tmpfs",
      "fstype": "tmpfs",
      "opts": "rw,nosuid,nodev"
    },
    {
      "target": "/sys/fs/cgroup/perf_event",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,perf_event"
    },
    {
      "target": "/sys/kernel/security",
      "source": "securityfs",
      "fstype": "securityfs",
      "opts": "rw,nosuid,nodev,noexec,relatime"
    },
    {
      "target": "/dev",
      "source": "devtmpfs",
      "fstype": "devtmpfs",
      "opts": "rw,nosuid,size=930516k,nr_inodes=232629,mode=755"
    },
    {
      "target": "/root/lab",
      "source": "auto.direct",
      "fstype": "autofs",
      "opts": "rw,relatime,fd=19,pgrp=1188,timeout=300,minproto=5,maxproto=5,direct,pipe_ino=19956"
    },
    {
      "target": "/sys/kernel/debug",
      "source": "debugfs",
      "fstype": "debugfs",
      "opts": "rw,relatime"
    },
    {
      "target": "/sys/fs/cgroup/freezer",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,freezer"
    },
    {
      "target": "/run/user/0",
      "source": "tmpfs",
      "fstype": "tmpfs",
      "opts": "rw,nosuid,nodev,relatime,size=188348k,mode=700"
    },
    {
      "target": "/",
      "source": "/dev/mapper/vg_00-lv_root",
      "fstype": "ext4",
      "opts": "rw,relatime,data=ordered"
    },
    {
      "target": "/var/lib/nfs/rpc_pipefs",
      "source": "sunrpc",
      "fstype": "rpc_pipefs",
      "opts": "rw,relatime"
    },
    {
      "target": "/proc/fs/nfsd",
      "source": "nfsd",
      "fstype": "nfsd",
      "opts": "rw,relatime"
    },
    {
      "target": "/sys/fs/cgroup",
      "source": "tmpfs",
      "fstype": "tmpfs",
      "opts": "ro,nosuid,nodev,noexec,mode=755"
    },
    {
      "target": "/sys/fs/cgroup/memory",
      "source": "cgroup",
      "fstype": "cgroup",
      "opts": "rw,nosuid,nodev,noexec,relatime,memory"
    },
    {
      "target": "/root/chef",
      "source": "auto.direct",
      "fstype": "autofs",
      "opts": "rw,relatime,fd=19,pgrp=1188,timeout=300,minproto=5,maxproto=5,direct,pipe_ino=19956"
    },
    {
      "target": "/misc",
      "source": "/etc/auto.misc",
      "fstype": "autofs",
      "opts": "rw,relatime,fd=7,pgrp=1188,timeout=300,minproto=5,maxproto=5,indirect,pipe_ino=19944"
    },
    {
      "target": "/dev/mqueue",
      "source": "mqueue",
      "fstype": "mqueue",
      "opts": "rw,relatime"
    },
    {
      "target": "/boot",
      "source": "/dev/vda1",
      "fstype": "ext3",
      "opts": "rw,relatime,data=ordered"
    },
    {
      "target": "/dev/hugepages",
      "source": "hugetlbfs",
      "fstype": "hugetlbfs",
      "opts": "rw,relatime"
    },
    {
      "target": "/testPool",
      "source": "testPool",
      "fstype": "zfs",
      "opts": "rw,xattr,noacl"
    },
    {
      "target": "/sys/kernel/config",
      "source": "configfs",
      "fstype": "configfs",
      "opts": "rw,relatime"
    }
  ]
}
"""
