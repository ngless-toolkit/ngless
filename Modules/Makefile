MODULES: motus specI targzs

MOTUS_VERSION = 0.1
SPECI_VERSION = 0.1

motus:
	cd motus.ngm/$(MOTUS_VERSION) && $(MAKE)

specI:
	cd specI.ngm/$(SPECI_VERSION) && $(MAKE)

targzs: packages/example-cmd-0.0.tar.gz packages/motus-$(MOTUS_VERSION).tar.gz packages/specI-$(SPECI_VERSION).tar.gz

packages/example-cmd-0.0.tar.gz:
	mkdir -p packages
	cd .. && tar cfz Modules/packages/example-cmd-0.0.tar.gz Modules/example-cmd.ngm/0.0

packages/motus-$(MOTUS_VERSION).tar.gz:
	cd .. && tar cfz Modules/packages/motus-$(MOTUS_VERSION).tar.gz Modules/motus.ngm/$(MOTUS_VERSION)

packages/specI-$(SPECI_VERSION).tar.gz:
	mkdir -p packages
	cd .. && tar cfz Modules/packages/specI-$(SPECI_VERSION).tar.gz Modules/specI.ngm/$(SPECI_VERSION)

.PHONY: MODULES packages motus
