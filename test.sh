export NAME_SUFFIX=$(pwd | sha256sum | head -c 32)
vagrant destroy -f device-scanner$NAME_SUFFIX manager$NAME_SUFFIX test$NAME_SUFFIX
vagrant up device-scanner$NAME_SUFFIX manager$NAME_SUFFIX test$NAME_SUFFIX
vagrant destroy -f device-scanner$NAME_SUFFIX manager$NAME_SUFFIX test$NAME_SUFFIX
