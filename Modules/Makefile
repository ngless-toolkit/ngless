MODULES: motus targzs

MOTUS_VERSION = 0.1

motus:
	cd motus.ngm/0.1 && $(MAKE)

targzs: packages/example-cmd-0.0.tar.gz packages/motus-0.1.tar.gz

packages/example-cmd-0.0.tar.gz:
	mkdir -p packages
	cd .. && tar cfz Modules/packages/example-cmd-0.0.tar.gz Modules/example-cmd.ngm/0.0

packages/motus-$(MOTUS_VERSION).tar.gz:
	cd .. && tar cfz Modules/packages/motus-$(MOTUS_VERSION).tar.gz Modules/motus.ngm/$(MOTUS_VERSION)

.PHONY: MODULES packages motus
