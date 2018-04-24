base_dir=$(abspath .)
lib_dir=$(base_dir)/lib

SRC_DIR=$(base_dir)/src/main/scala

ROCKETCHIP_DIR=$(base_dir)/rocket-chip
EXTRA_PACKAGES=testchipip

rocketchip_stamp=$(base_dir)/lib/rocketchip.stamp
SBT ?= java -Xmx2G -Xss8M -XX:MaxPermSize=256M -jar $(ROCKETCHIP_DIR)/sbt-launch.jar
#extra_stamps = $(addprefix $(lib_dir)/,$(addsuffix .stamp,$(EXTRA_PACKAGES)))

lookup_scala_srcs = $(shell find $(1)/ -iname "*.scala" 2> /dev/null)

verilog: $(call lookup_scala_srcs, $(SRC_DIR)) libs
	$(SBT) "run-main fault_detector.FaultDetector"

libs: $(rocketchip_stamp) #$(extra_stamps)

FIRRTL_JAR ?= $(ROCKETCHIP_DIR)/firrtl/utils/bin/firrtl.jar
FIRRTL ?= java -Xmx2G -Xss8M -XX:MaxPermSize=256M -cp $(FIRRTL_JAR) firrtl.Driver

$(rocketchip_stamp): $(call lookup_scala_srcs, $(ROCKETCHIP_DIR)) $(FIRRTL_JAR)
	cd $(ROCKETCHIP_DIR) && $(SBT) pack
	mkdir -p $(lib_dir)
	cp $(ROCKETCHIP_DIR)/target/pack/lib/*.jar $(lib_dir)
	touch $(rocketchip_stamp)

-include $(base_dir)/Makefrag.pkgs

$(base_dir)/Makefrag.pkgs: $(base_dir)/generate-pkg-mk.sh
	bash $(base_dir)/generate-pkg-mk.sh $(EXTRA_PACKAGES) > $@

$(FIRRTL_JAR): $(call lookup_scala_srcs, $(ROCKETCHIP_DIR)/firrtl/src/main/scala)
	$(MAKE) -C $(ROCKETCHIP_DIR)/firrtl SBT="$(SBT)" root_dir=$(ROCKETCHIP_DIR)/firrtl build-scala
	mkdir -p $(ROCKETCHIP_DIR)/lib
	cp -p $(FIRRTL_JAR) $(ROCKETCHIP_DIR)/lib

