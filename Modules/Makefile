MODULES: motus targzs

motus:
	cd motus.ngm/0.0 && $(MAKE)

targzs: packages/example-cmd-0.0.tar.gz packages/motus-0.0.tar.gz

packages/example-cmd-0.0.tar.gz:
	mkdir -p packages
	cd .. && tar cfz Modules/packages/example-cmd-0.0.tar.gz Modules/example-cmd.ngm/0.0

packages/motus-0.0.tar.gz: motus
	cd .. && tar cfz Modules/packages/motus-0.0.tar.gz Modules/motus.ngm/0.0

.PHONY: MODULES packages motus
