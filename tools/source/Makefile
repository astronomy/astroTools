#FORT = lf95
#LFLAGS = --wide -O -lpgplot -lX11 -lpng 
#LFLAGS = --info --wide -O -lpgplot -lX11 -lpng 

FORT = ifort
#CFLAGS = -132 -O0 #-vec-report0 
CFLAGS = -free -O0 -vec-report0 
#LFLAGS = -lpgplot_ifort -L/usr/X11R6/lib -lX11 -lm -L/opt/local/lib -laquaterm -Wl,-framework -Wl,Foundation -lifcore -limf -lirc -lsvml -lifport -Xlinker -multiply_defined suppress -bind_at_load 
#LFLAGS = -lpgplot_ifort -L/usr/X11R6/lib -lX11 -lm -L/opt/local/lib -Wl,-framework -Wl,Foundation -lifcore -limf -lirc -lsvml -lifport -Xlinker -multiply_defined suppress -bind_at_load 
LFLAGS = -lpgplot_ifort -L/usr/X11R6/lib -lX11 -lm #-L/opt/local/lib -Wl,-framework -Wl,Foundation -lifcore -limf -lirc -lsvml -lifport -Xlinker -multiply_defined suppress -bind_at_load 


STDOBJ = functions.o

.f.o: 
	$(FORT) $(CFLAGS) -c $? 

exit:


all: accuracy addmagnitudes arcsec2rad az2wd cal2jd comet_eq2ap ddd2dms deg2rad deltat distance_in_sky distance_on_earth dms2ddd dms2rad hms2rad ecl2eq eq2ecl eq2gal gal2eq gps2cal jd2cal moondiam precess rad2deg timestamp2cal

accuracy: $(STDOBJ) accuracy.o
	$(FORT) $(LFLAGS) -o ../accuracy $(STDOBJ) accuracy.o

addmagnitudes: $(STDOBJ) addmagnitudes.o
	$(FORT) $(LFLAGS) -o ../addmagnitudes $(STDOBJ) addmagnitudes.o

arcsec2rad: $(STDOBJ) arcsec2rad.o
	$(FORT) $(LFLAGS) -o ../arcsec2rad $(STDOBJ) arcsec2rad.o

az2wd: $(STDOBJ) az2wd.o
	$(FORT) $(LFLAGS) -o ../az2wd $(STDOBJ) az2wd.o

cal2jd: $(STDOBJ) cal2jd.o
	$(FORT) $(LFLAGS) -o ../cal2jd $(STDOBJ) cal2jd.o

comet_eq2ap: $(STDOBJ) comet_eq2ap.o
	$(FORT) $(LFLAGS) -o ../comet_eq2ap $(STDOBJ) comet_eq2ap.o

ddd2dms: $(STDOBJ) ddd2dms.o
	$(FORT) $(LFLAGS) -o ../ddd2dms $(STDOBJ) ddd2dms.o

deg2rad: $(STDOBJ) deg2rad.o
	$(FORT) $(LFLAGS) -o ../deg2rad $(STDOBJ) deg2rad.o

deltat: $(STDOBJ) deltat.o
	$(FORT) $(LFLAGS) -o ../deltat $(STDOBJ) deltat.o

distance_in_sky: $(STDOBJ) distance_in_sky.o
	$(FORT) $(LFLAGS) -o ../distance_in_sky $(STDOBJ) distance_in_sky.o

distance_on_earth: $(STDOBJ) distance_on_earth.o
	$(FORT) $(LFLAGS) -o ../distance_on_earth $(STDOBJ) distance_on_earth.o

dms2ddd: $(STDOBJ) dms2ddd.o
	$(FORT) $(LFLAGS) -o ../dms2ddd $(STDOBJ) dms2ddd.o

dms2rad: $(STDOBJ) dms2rad.o
	$(FORT) $(LFLAGS) -o ../dms2rad $(STDOBJ) dms2rad.o

eq2ecl: $(STDOBJ) eq2ecl.o
	$(FORT) $(LFLAGS) -o ../eq2ecl $(STDOBJ) eq2ecl.o

ecl2eq: $(STDOBJ) ecl2eq.o
	$(FORT) $(LFLAGS) -o ../ecl2eq $(STDOBJ) ecl2eq.o

eq2gal: $(STDOBJ) eq2gal.o
	$(FORT) $(LFLAGS) -o ../eq2gal $(STDOBJ) eq2gal.o

gal2eq: $(STDOBJ) gal2eq.o
	$(FORT) $(LFLAGS) -o ../gal2eq $(STDOBJ) gal2eq.o

gps2cal: $(STDOBJ) gps2cal.o
	$(FORT) $(LFLAGS) -o ../gps2cal $(STDOBJ) gps2cal.o

hms2rad: $(STDOBJ) hms2rad.o
	$(FORT) $(LFLAGS) -o ../hms2rad $(STDOBJ) hms2rad.o

jd2cal: $(STDOBJ) jd2cal.o
	$(FORT) $(LFLAGS) -o ../jd2cal $(STDOBJ) jd2cal.o

moondiam: $(STDOBJ) moondiam.o
	$(FORT) $(LFLAGS) -o ../moondiam $(STDOBJ) moondiam.o

julian: $(STDOBJ) julian.o
	$(FORT) $(LFLAGS) -o ../julian $(STDOBJ) julian.o

precess: $(STDOBJ) precess.o
	$(FORT) $(LFLAGS) -o ../precess $(STDOBJ) precess.o

rad2deg: $(STDOBJ) rad2deg.o
	$(FORT) $(LFLAGS) -o ../rad2deg $(STDOBJ) rad2deg.o

timestamp2cal: $(STDOBJ) timestamp2cal.o
	$(FORT) $(LFLAGS) -o ../timestamp2cal $(STDOBJ) timestamp2cal.o





clean:
	rm -f *.o *.mod *.exe
