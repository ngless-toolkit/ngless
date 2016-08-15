OM-RGC.fna.gz:
	wget http://vm-lux.embl.de/~kultima/share/MOCAT/v2.0/OM-RGC.zip
	unzip OM-RGC.zip
	cat OM-RGC.1.coord OM-RGC.2.coord OM-RGC.3.coord OM-RGC.4.coord OM-RGC.5.coord OM-RGC.6.coord OM-RGC.7.coord OM-RGC.8.coord OM-RGC.9.coord > OM-RGC.fna.coord
	rm OM-RGC.1.coord OM-RGC.2.coord OM-RGC.3.coord OM-RGC.4.coord OM-RGC.5.coord OM-RGC.6.coord OM-RGC.7.coord OM-RGC.8.coord OM-RGC.9.coord
	mv OM-RGC.1-9.functional.map OM-RGC.functional.map
	cat OM-RGC.1 OM-RGC.2 OM-RGC.3 OM-RGC.4 OM-RGC.5 OM-RGC.6 OM-RGC.7 OM-RGC.8 OM-RGC.9 | gzip > OM-RGC.fna.gz
	rm OM-RGC.1 OM-RGC.2 OM-RGC.3 OM-RGC.4 OM-RGC.5 OM-RGC.6 OM-RGC.7 OM-RGC.8 OM-RGC.9
	rm OM-RGC.zip

OM-RGC.tar.gz: OM-RGC.fna.gz
	/g/bork1/coelho/prefix/bin/ngless --create-reference-pack --output-name $@ --genome-url OM-RGC.fna.gz --functional-map-url OM-RGC.functional.map
