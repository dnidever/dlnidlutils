pro fits_grid_convert,head,inpsys,outsys,xx,yy,lon,lat,reverse=reverse

; Convert from input system to output system

; X/Y -> LON/LAT
;------------------
if not keyword_set(reverse) then begin

  ; Convert to output coordinate system
  ;   the output coordinates are always called LON/LAT
  if outsys ne inpsys then begin

    ; Input coordinate system
    CASE inpsys of
    ; Input Galactic
    1: begin

      XYAD,head,xx,yy,glon,glat

      case outsys of
      ; Output Celestial
      2: GLACTC,lon,lat,2000.0,glon,glat,2,/deg
      ; Output Magellanic
      3: GAL2MAG,glon,glat,lon,lat,wrap=0
      else: stop,'Not supported'
      endcase

    end
    ; Celestial
    2: begin

      XYAD,head,xx,yy,ra,dec

      case outsys of
      ; Output Galactic
      1: GLACTC,ra,dec,2000.0,lon,lat,1,/deg
      2: begin
          ra=lon & dec=lat
         end
      ; Output Magellanic
      3: begin
        GLACTC,ra,dec,2000.0,glon,glat,1,/deg
        GAL2MAG,glon,glat,lon,lat,wrap=0
      end
      else: stop,'Not supported'
      endcase

    end
    ; Magellanic
    3: begin

      XYAD,head,xx,yy,mlon,mlat

      case outsys of
      ; Output Galactic
      1:  MAG2GAL,mlon,mlat,lon,lat
      ; Output Celestial
      2: begin
         MAG2GAL,mlon,mlat,glon,glat
         GLACTC,lon,lat,2000.0,glon,glat,2,/deg
      end
      else: stop,'Not supported'
      endcase

    end
    else:  stop,'Not supported'

    ENDCASE

  ; Output same as input
  endif else begin
    XYAD,head,xx,yy,lon,lat
  endelse

; LON/LAT -> X/Y
;------------------
endif else begin

  ; Convert from OUTPUT to INPUT coordinate system
  ;   the OUTPUT coordinates are always called LON/LAT
  if outsys ne inpsys then begin

    ; Input coordinate system
    CASE inpsys of
    ; Input Galactic
    1: begin

      case outsys of
      ; Output Celestial
      2: GLACTC,lon,lat,2000.0,glon,glat,1,/deg
      ; Output Magellanic
      3: MAG2GAL,lon,lat,glon,glat
      else: stop,'Not supported'
      endcase

      ADXY,head,glon,glat,xx,yy

    end
    ; Celestial
    2: begin

      case outsys of
      ; Output Galactic
      1: GLACTC,ra,dec,2000.0,lon,lat,2,/deg
      ; Output Magellanic
      3: begin
        MAG2GAL,lon,lat,glon,glat
        GLACTC,ra,dec,2000.0,glon,glat,2,/deg
      end
      else: stop,'Not supported'
      endcase

      ADXY,head,ra,dec,xx,yy

    end
    ; Magellanic
    3: begin

      case outsys of
      ; Output Galactic
      1:  GAL2MAG,lon,lat,mlon,mlat,wrap=0
      ; Output Celestial
      2: begin
         GLACTC,lon,lat,2000.0,glon,glat,1,/deg
         GAL2MAG,glon,glat,mlon,mlat,wrap=0
      end
      else: stop,'Not supported'
      endcase

      ADXY,head,mlon,mlat,xx,yy

    end
    else:  stop,'Not supported'

    ENDCASE

  ; Output same as input
  endif else begin
    ADXY,head,lon,lat,xx,yy
  endelse


endelse


end

;--------------------------

pro fits_grid,head,position=position,color=color,xtit=xtit,ytit=ytit,$
              ralevels=ralevels,declevels=declevels,stp=stp,$
              xtickname=xtickname,ytickname=ytickname,thick=thick,$
              raticklevels=raticklevels,decticklevels=decticklevels,$
              nodecticklevels=nodecticklevels,noraticklevels=noraticklevels,$
              magellanic=magellanic,framecolor=framecolor,charsize=charsize,$
              transp=transp,linestyle=linestyle,xflip=xflip,yflip=yflip,$
              galactic=galactic,celestial=celestial,lonlevels=lonlevels,$
              latlevels=latlevels,lonticklevels=lonticklevels,latticklevels=latticklevels,$
              nolonticklevels=nolonticklevels,nolatticklevels=nolatticklevels,$
              hours=hours,translabels=translabels,lontickname=lontickname,$
              lattickname=lattickname,lontitle=lontitle,lattitle=lattitle,$
              lonlabelsflip=lonlabelsflip,latlabelsflip=latlabelsflip,$
              lontickformat=lontickformat,lattickformat=lattickformat

;+
;
; FITS_GRID
;
; This program overplots a grid over an image using
; the FITS header WCS coordinate system.
;
; It's best to only make a box (if that) before fits_grid
; is called.  In order to do that use:
;   xtickformat='(A1)',ytickformat='(A1)',xticks=1,yticks=1
;
; INPUTS:
;  head              FITS header with WCS coordinate system
;  /galactic         Use the Galactic coordinate system for the grid
;  /celestial        Use the Celestia coordinate system for the grid
;  /magellanic       Use the Magellanic Stream coordinate system (from
;                      Nidever et al.2008) for the grid
;  =lonlevels        Array of LON/RA/MSLON levels to use for the grid lines
;  =latlevels        Array of LAT/DEC/MSLAT levels to use for the grid lines
;  =lontitle         The LON title
;  =lattitle         The LAT title
;  =lonticklevels    Array of LON/RA/MSLON levels for annotations
;  =latticklevels    Array of LAT/DEC/MSLAT levels for annotations
;  =lontickname      The annotations to use with LONTICKLEVELS.
;  =lattickanme      The annotations to use with LATTICKLEVELS.
;  /nolonticklevels  No LON/RA/MSLON annotations
;  /nolatticklevels  No LAT/DEC/MSLAT annotations
;  /transp           Transpose the coordinates, i.e. LON vs. LAT instead of LAT vs. LON.
;  /translabels      Flip the axes on which the LON/LAT annotations
;                      are plotted (e.g. LAT labels on X-axis and LON
;                      labels on Y-axis if transp=0).
;  /lonlabelsflip    Put the LON annotations on the TOP or RIGHT side
;                      of the plot
;  /latlabelsflip    Put the LAT annotations on the TOP or RIGHT side
;                      of the plot
;  =thick            The thickness of the gridlines.
;  =position         Where the plot is positioned 
;  =color            The color of the grid lines.  Grey-scale 150 by default.
;  =framecolor       Color of the frame
;  =linestyle        The linestyle to use for the grid lines
;  /xflip            Reverse the direction of the X-axis
;  /yflip            Reverse the direction of the Y-axis
;  /hours            RA in hours.  This is the default for RA.
;  /stp              Stop at the end of the program
;
;  DEPRICATED INPUTS:
;  =ralevels       Array of RA levels to use for the grid
;  =declevels      Array of DEC levels to use for the grid
;  =raticklevels   Array of RA levels for annotations
;  =decticklevels  Array of DEC levels for annotations
;  /nodecticklevels   No DEC annotations
;  /noraticklevels    No RA annotations
;  =xtickname
;  =ytickname
;  =xtit
;  =ytit
;
;  REMOVED KEYWORDS:
;  =xx
;  =yy
;
; OUTPUTS:
;  Overplots a coordinate grid
;
; By D.Nidever  November 2006
;   rewritten June 2009   D.Nidever
;-

; Not enough inputs
if n_params() eq 0 then begin
  print,'Syntax - fits_grid,head,position=position,color=color,lontit=lontit,lattit=lattit,'
  print,'            lonlevels=lonlevels,latlevels=latlevels,stp=stp,'
  print,'            lontickname=lontickname,lattickname=lattickname,thick=thick,'
  print,'            lonticklevels=lonticklevels,latticklevels=latticklevels,'
  print,'            linestyle=linestyle,xflip=xflip,yflip=yflip,galactic=galactic,'
  print,'            celestial=celestial,magellanic=magellanic,translabels=translabels'
  print,'            lonlabelsflip=lonlabelsflip,latlabelsflip=latlabelsflip'
  return
endif

; Depricated keyword inputs
if keyword_set(ralevels) and not keyword_set(lonlevels) then lonlevels=ralevels
if keyword_set(declevels) and not keyword_set(latlevels) then latlevels=declevels
if keyword_set(raticklevels) and not keyword_set(lonticklevels) then lonticklevels=raticklevels
if keyword_set(decticklevels) and not keyword_set(latticklevels) then latticklevels=decticklevels
if keyword_set(noraticklevels) and not keyword_set(nolonticklevels) then nolonticklevels=noraticklevels
if keyword_set(nodecticklevels) and not keyword_set(nolatticklevels) then nolatticklevels=nodecticklevels
if keyword_set(xtickname) and not keyword_set(lontickname) then lontickname=xtickname
if keyword_set(ytickname) and not keyword_set(lattickname) then lattickname=ytickname


if not keyword_set(position) then begin
  xwin = !x.window
  ywin = !y.window

  ; No coordinate system set yet
  if total(!x.window) eq 0 or total(!y.window) eq 0 then begin

    ; Make a dummy plot
    ;  this is from display.pro
    old_set = !d.name
    set_plot,'X'
    plot,[0,1],/NoErase,/NoData,XStyle=4,YStyle=4,charsize=charsize
    set_plot,old_set

  end

  position = [!x.window[0],!y.window[0],!x.window[1],!y.window[1]]

  ;position = [0.0998386, 0.0750519, 0.970055, 0.962482]
endif

; Sizes
nx = sxpar(head,'NAXIS1')
ny = sxpar(head,'NAXIS2')

; X/Y ranges
minx = 0L
maxx = nx-1L
miny = 0L
maxy = ny-1L

; Steps
cdelt1 = SXPAR(head,'CDELT1',count=ncdelt1)
if ncdelt1 eq 0 then cdelt1 = SXPAR(head,'CD1_1')
cdelt2 = SXPAR(head,'CDELT2',count=ncdelt2)
if ncdelt2 eq 0 then cdelt2 = SXPAR(head,'CD2_2')
cdelt = cdelt1 > cdelt2

; Axis labels
cx = sxpar(head,'CTYPE1')
cy = sxpar(head,'CTYPE2')
;if n_elements(xtit) eq 0 then xtit=cx
;if n_elements(ytit) eq 0 then ytit=cy

; Input coordinate system
;  galactic, celestial, magellanic
inpsys = -1
if strmid(strupcase(cx),0,4) eq 'GLON' or strmid(strupcase(cx),0,4) eq 'GLAT' then inpsys=1
if strmid(strupcase(cx),0,2) eq 'RA' or strmid(strupcase(cx),0,3) eq 'DEC' then inpsys=2
if strmid(strupcase(cx),0,5) eq 'MSLON' then inpsys=3
if inpsys eq -1 then begin
  print,'Coordinate System >>',cx,'<< NOT supported'
  return
endif

; Output coordinate system
;  galactic, celestial, magellanic
outsys = -1
if keyword_set(galactic) then outsys=1
if keyword_set(celestial) then outsys=2
if keyword_set(magellanic) then outsys=3
if outsys eq -1 then begin
  print,'No output coordinate system given. Using Celestial'
  outsys = 2
endif

; Getting output coordinate system names
case outsys of
1: begin
     outlon_name='GLON'
     outlat_name='GLAT'
   end
2: begin
     outlon_name='RA'
     outlat_name='DEC'
   end
3: begin
     outlon_name='MSLON'
     outlat_name='MSLAT'
   end
else:
endcase

if n_elements(lontitle) eq 0 then lontitle=outlon_name
if n_elements(lattitle) eq 0 then lattitle=outlat_name


; Getting the X/Y arrays
;if not keyword_set(xx) then $
;  xx = findgen(nx)#(fltarr(ny)+1.0)
;if not keyword_set(yy) then $
;  yy = (fltarr(nx)+1.0)#findgen(ny)

; Coordinates for the perimeter
xx = [ findgen(nx), fltarr(ny)+nx-1L, reverse(findgen(nx)), fltarr(ny) ]
yy = [ fltarr(nx), findgen(ny), fltarr(nx)+ny-1L, reverse(findgen(ny)) ]

; Get the coordinates
;---------------------
FITS_GRID_CONVERT,head,inpsys,outsys,xx,yy,lon,lat


; LON/LAT ranges
minlon = min(lon)
maxlon = max(lon)

; crossing LON=0
if (minlon lt 2.0*cdelt and maxlon gt (360.0-2.0*cdelt)) then begin
  bd = where(lon gt 180.0,nbd)
  lon[bd] = lon[bd]-360.0
  minlon = min(lon)
  maxlon = max(lon)
endif
;if (lon[nx-1,0] lt lon[0,0]) then begin
;  bd = where(lon gt min(lon[0,*])-0.1,nbd)
;  lon[bd] = lon[bd]-24.0
;  minlon = min(lon)
;  maxlon = max(lon)
;endif
minlat = min(lat)
maxlat = max(lat)

; RA in hours
lonfac = 1.0d0
if n_elements(hours) eq 0 then hours=1
if outsys eq 2 then begin
  lonfac = 15.0d0
  lon = lon/lonfac
endif

; The levels
if n_elements(lonlevels) eq 0 then begin
  nlon = 7
  dlon = (maxlon-minlon)/(nlon-1)
  lonlevels = findgen(nlon)*dlon+minlon
end
nlon = n_elements(lonlevels)
if n_elements(latlevels) eq 0 then begin  nlat = 7
  nlat = 7
  dlat = (maxlat-minlat)/(nlat-1)
  latlevels = findgen(nlat)*dlat+minlat
end
nlat = n_elements(latlevels)


;loadct,0,/silent
if n_elements(color) eq 0 then color=150
if n_elements(framecolor) eq 0 then framecolor=0


; Initializing the coordinate arrays
n = 300
xlon_arr = fltarr(nlon,n)
ylon_arr = fltarr(nlon,n)
xlat_arr = fltarr(nlat,n)
ylat_arr = fltarr(nlat,n)

; Getting LON grid lines
for i=0,n_elements(lonlevels)-1 do begin
  ilon = lonlevels[i]*lonfac
  ;adxy,head,(fltarr(n)+ilon)*15.,scale_vector(findgen(n),minlat-1,maxlat+1),x,y
  FITS_GRID_CONVERT,head,inpsys,outsys,x,y,(fltarr(n)+ilon),$
                    scale_vector(findgen(n),minlat-1,maxlat+1),/reverse
  xlon_arr[i,*] = x
  ylon_arr[i,*] = y
end

; Getting LAT grid lines
for i=0,n_elements(latlevels)-1 do begin
  ilat = latlevels[i]
  ;adxy,head,scale_vector(findgen(n),minlon-1,maxlon+1)*15.,fltarr(n)+ilat,x,y
  FITS_GRID_CONVERT,head,inpsys,outsys,x,y,scale_vector(findgen(n),minlon-1,maxlon+1),$
                    fltarr(n)+ilat,/reverse
  xlat_arr[i,*] = x
  ylat_arr[i,*] = y
end



if n_elements(lonticklevels) eq 0 then lonticklevels=lonlevels
if n_elements(latticklevels) eq 0 then latticklevels=latlevels


; Annotations
if n_elements(lontickformat) gt 0 and n_elements(lontickname) eq 0 then lontickname=strtrim(string(lonticklevels,format=lontickformat),2)
if n_elements(lontickname) eq 0 then $
  lontickname = stringize(lonticklevels,ndec=1)
if n_elements(lattickformat) gt 0 and n_elements(lattickname) eq 0 then lattickname=strtrim(string(latticklevels,format=lontickformat),2)
if n_elements(lattickname) eq 0 then $
  lattickname = stringize(latticklevels,ndec=1)


;-----------------------
; Normal Y vs. X plot
;-----------------------
if not keyword_set(transp) then begin


  undefine,xlon,ylon,xlat,ylat

  ; Getting LON label positions
  for i=0,n_elements(lonticklevels)-1 do begin
    ilon = lonticklevels[i]*lonfac
    ;adxy,head,(fltarr(n)+ilon)*15.,scale_vector(findgen(n),minlat-1,maxlat+1),x,y
    FITS_GRID_CONVERT,head,inpsys,outsys,x,y,(fltarr(n)+ilon),$
                      scale_vector(findgen(n),minlat-1,maxlat+1),/reverse

    ; Only want points on the grid
    gd = where(x ge minx and x le maxx and y ge miny and y le maxy,ngd)

    ; Some points on the grid
    if ngd gt 0 then begin

      ; Normal, LON on X-axis
      if not keyword_set(translabels) then begin
        yline = miny
        if keyword_set(yflip) xor keyword_set(lonlabelsflip) then yline=maxy
        dum = closest(yline,y[gd],ind=ind)
        xadd = x[gd[ind]]
        yadd = y[gd[ind]]
        ; must be on the boundary
        if abs(yline-y[gd[ind]]) gt 10 then yadd=miny-1000

      ; TRANSLABEL, LON on Y-axis
      endif else begin
        xline = minx
        if keyword_set(xflip) xor keyword_set(lonlabelsflip) then xline=maxx
        dum = closest(xline,x[gd],ind=ind)
        xadd = x[gd[ind]]
        yadd = y[gd[ind]]
        ; must be on the boundary
        if abs(xline-x[gd[ind]]) gt 10 then xadd=minx-1000

      endelse
      push,xlon,xadd
      push,ylon,yadd

    ; No points on the grid
    endif else begin
      push,xlon,minx-1000
      push,ylon,miny-1000
    endelse


  endfor ; LON label positions loop

  ; Getting LAT label positions
  for i=0,n_elements(latticklevels)-1 do begin
    ilat = latticklevels[i]
    ;adxy,head,scale_vector(findgen(n),minlon-1,maxlon+1)*15.,fltarr(n)+ilat,x,y
    FITS_GRID_CONVERT,head,inpsys,outsys,x,y,scale_vector(findgen(n),minlon-1,maxlon+1),$
                      fltarr(n)+ilat,/reverse

    ; Only want points on the grid
    gd = where(x ge minx and x le maxx and y ge miny and y le maxy,ngd)

    ; Some points on the grid
    if ngd gt 0 then begin

      ; Normal, LAT on Y-axis
      if not keyword_set(translabels) then begin
        xline = minx
        if keyword_set(xflip) xor keyword_set(latlabelsflip) then xline=maxx
        dum = closest(xline,x[gd],ind=ind)
        xadd = x[gd[ind]]
        yadd = y[gd[ind]]
        ; must be on the boundary
        if abs(xline-x[gd[ind]]) gt 10 then xadd=minx-1000

      ; TRANSLABEL, LAT on X-axis
      endif else begin
        yline = miny
        if keyword_set(yflip) xor keyword_set(latlabelsflip) then yline=maxy
        dum = closest(yline,y[gd],ind=ind)
        xadd = x[gd[ind]]
        yadd = y[gd[ind]]
        ; must be on the boundary
        if abs(yline-y[gd[ind]]) gt 10 then yadd=miny-1000
      endelse
      push,xlat,xadd
      push,ylat,yadd

    ; No points on the grid
    endif else begin
      push,ylat,miny-1000
      push,xlat,minx-1000
    endelse

  endfor ; LAT label positions loop

  ; Only getting the good LON levels
  gdlon = where(xlon ge minx and xlon lt maxx and ylon ge miny and ylon le maxy,ngdlon)
  ; Only getting the good LAT levels
  gdlat = where(xlat ge minx and xlat le maxx and ylat ge miny and ylat lt maxy,ngdlat)
  
  ;stop

  if ngdlon eq 0 then begin
    print,'NO GOOD LON POINTS'
    return
  endif
  if ngdlat eq 0 then begin
    print,'NO GOOD LAT POINTS'
    return
  endif


  ; Normal labels, LON labels on X, LAT labels on Y
  xaxis = 0
  yaxis = 0
  if not keyword_set(translabels) then begin
    xtickn = lontickname[gdlon]
    xtickv = xlon[gdlon]
    ytickn = lattickname[gdlat]
    ytickv = ylat[gdlat]

    if keyword_set(nolonticklevels) then xtickf='(A1)'
    if keyword_set(nolatticklevels) then ytickf='(A1)'

    if not keyword_set(xtit) then xtit=lontitle
    if not keyword_set(ytit) then ytit=lattitle

    if keyword_set(lonlabelsflip) then xaxis=1
    if keyword_set(latlabelsflip) then yaxis=1
    
  ; Translabels, LON labels on Y, LAT labels on X
  endif else begin
    xtickn = lattickname[gdlat]
    xtickv = xlat[gdlat]
    ytickn = lontickname[gdlon]
    ytickv = ylon[gdlon]

    if keyword_set(nolonticklevels) then ytickf='(A1)'
    if keyword_set(nolatticklevels) then xtickf='(A1)'

    if not keyword_set(xtit) then xtit=lattitle
    if not keyword_set(ytit) then ytit=lontitle

    if keyword_set(lonlabelsflip) then yaxis=1
    if keyword_set(latlabelsflip) then xaxis=1
  endelse
  xticks = n_elements(xtickn)-1
  yticks = n_elements(ytickn)-1


  ; X/Y ranges
  xr = [minx,maxx]
  if keyword_set(xflip) then xr=reverse(xr)
  yr = [miny,maxy]
  if keyword_set(yflip) then yr=reverse(yr)

  ;plot,dist(5),/nodata,/noerase,xtickname=xtickn,xtickv=xtickv,ytickname=ytickn,$
  ;     ytickv=ytickv,position=position,xminor=1,yminor=1,xticklen=0.0001,yticklen=0.0001,$
  ;     xticks=xticks,yticks=yticks,xr=xr,yr=yr,xs=1,ys=1,xtit=xtit,ytit=ytit,$
  ;     xtickformat=xtickf,ytickformat=ytickf,color=framecolor,charsize=charsize

  ; Setup coordinate system
  plot,[0],[0],/nodata,/noerase,xs=1,ys=1,xr=xr,yr=yr,position=position,xminor=1,yminor=1,$
       xtickformat='(A1)',ytickformat='(A1)',xticks=1,yticks=1
  ; Use axis to plot the annotations
  AXIS,xaxis=xaxis,xtickname=xtickn,xtickv=xtickv,xminor=1,xticklen=0.0001,xticks=xticks,$
       xtit=xtit,xtickformat=xtickf,color=framecolor,charsize=charsize,xs=1
  AXIS,yaxis=yaxis,ytickname=ytickn,ytickv=ytickv,yminor=1,yticklen=0.0001,yticks=yticks,$
       ytit=ytit,ytickformat=ytickf,color=framecolor,charsize=charsize,ys=1

  ; Overplot the grid lines
  for i=0,nlon-1 do begin
    x1 = reform(xlon_arr[i,*])
    y1 = reform(ylon_arr[i,*])
    gdpts = where(x1 ge minx and x1 le maxx and y1 ge miny and y1 le maxy,ngdpts)
    if ngdpts gt 0 then $
      oplot,[x1[gdpts]],[y1[gdpts]],color=color,thick=thick,linestyle=linestyle
    ;oplot,xlon_arr[i,*],ylon_arr[i,*],color=color,thick=thick,linestyle=linestyle
  end
  for i=0,nlat-1 do begin
    x1 = reform(xlat_arr[i,*])
    y1 = reform(ylat_arr[i,*])
    gdpts = where(x1 ge minx and x1 le maxx and y1 ge miny and y1 le maxy,ngdpts)
    if ngdpts gt 0 then $
      oplot,[x1[gdpts]],[y1[gdpts]],color=color,thick=thick,linestyle=linestyle
    ;oplot,xlat_arr[i,*],ylat_arr[i,*],color=color,thick=thick,linestyle=linestyle
  end


;-------------------------------------
; Tranpose the coordinates, X vs. Y
;-------------------------------------
endif else begin


  undefine,xlon,ylon,xlat,ylat

  ; Getting LON label positions
  for i=0,n_elements(lonticklevels)-1 do begin
    ilon = lonticklevels[i]*lonfac
    ;adxy,head,(fltarr(n)+ilon)*15.,scale_vector(findgen(n),minlat-1,maxlat+1),x,y
    FITS_GRID_CONVERT,head,inpsys,outsys,x,y,(fltarr(n)+ilon),$
                      scale_vector(findgen(n),minlat-1,maxlat+1),/reverse

    ; Only want points on the grid
    gd = where(x ge minx and x le maxx and y ge miny and y le maxy,ngd)

    ; Some points on the grid
    if ngd gt 0 then begin

      ; Normal, LON on Y-axis
      if not keyword_set(translabels) then begin
        xline = minx
        if keyword_set(xflip) xor keyword_set(lonlabelsflip) then xline=maxx
        dum = closest(xline,x[gd],ind=ind)
        xadd = x[gd[ind]]
        yadd = y[gd[ind]]
        ; must be on the boundary
        if abs(xline-x[gd[ind]]) gt 10 then xadd=minx-1000

      ; TRANSLABEL, LON on X-axis
      endif else begin  
        yline = miny
        if keyword_set(yflip) xor keyword_set(lonlabelsflip) then yline=maxy
        dum = closest(yline,y[gd],ind=ind)
        xadd = x[gd[ind]]
        yadd = y[gd[ind]]
        ; must be on the boundary
        if abs(yline-y[gd[ind]]) gt 10 then yadd=miny-1000
      endelse
      push,xlon,xadd
      push,ylon,yadd

    ; No points on the grid 
    endif else begin
      push,xlon,minx-1000
      push,ylon,miny-1000
    endelse

  endfor ; LON label positions loop

  ; Getting LAT label positions
  for i=0,n_elements(latticklevels)-1 do begin
    ilat = latticklevels[i]
    ;adxy,head,scale_vector(findgen(n),minlon-1,maxlon+1)*15.,fltarr(n)+ilat,x,y
    FITS_GRID_CONVERT,head,inpsys,outsys,x,y,scale_vector(findgen(n),minlon-1,maxlon+1),$
                      fltarr(n)+ilat,/reverse

    ; Only want points on the grid
    gd = where(x ge minx and x le maxx and y ge miny and y le maxy,ngd)

    ; Some points on the grid
    if ngd gt 0 then begin

      ; Normal, LAT on X-axis
      if not keyword_set(translabels) then begin
        yline = miny
        if keyword_set(yflip) xor keyword_set(latlabelsflip) then yline=maxy
        dum = closest(yline,y[gd],ind=ind)
        xadd = x[gd[ind]]
        yadd = y[gd[ind]]
        ; must be on the boundary
        if abs(yline-y[gd[ind]]) gt 10 then yadd=miny-1000

      ; TRANSLABEL, LAT on Y-axis
      endif else begin
        xline = minx
        if keyword_set(xflip) xor keyword_set(latlabelsflip) then xline=maxx
        dum = closest(xline,x[gd],ind=ind)
        xadd = x[gd[ind]]
        yadd = y[gd[ind]]
        ; must be on the boundary
        if abs(xline-x[gd[ind]]) gt 10 then xadd=minx-1000
      endelse
      push,xlat,xadd
      push,ylat,yadd

    ; No points on the grid
    endif else begin
      push,ylat,miny-1000
      push,xlat,minx-1000
    endelse

  endfor ; LAT label positions loop

  ; Only getting the good LON levels
  gdlon = where(xlon ge minx and xlon lt maxx and ylon ge miny and ylon le maxy,ngdlon)
  ; Only getting the good LAT levels
  gdlat = where(xlat ge minx and xlat le maxx and ylat ge miny and ylat lt maxy,ngdlat)

  if ngdlon eq 0 then begin
    print,'NO GOOD LON POINTS'
    return
  endif
  if ngdlat eq 0 then begin
    print,'NO GOOD LAT POINTS'
    return
  endif


  ; Normal transp labels, LON labels on Y, LAT labels on X
  if not keyword_set(translabels) then begin
    xtickn = lattickname[gdlat]
    xtickv = xlat[gdlat]
    ytickn = lontickname[gdlon]
    ytickv = ylon[gdlon]

    if keyword_set(nolonticklevels) then ytickf='(A1)'
    if keyword_set(nolatticklevels) then xtickf='(A1)'

    if not keyword_set(xtit) then xtit=lattitle
    if not keyword_set(ytit) then ytit=lontitle

    if keyword_set(lonlabelsflip) then yaxis=1
    if keyword_set(latlabelsflip) then xaxis=1
    
  ; Translabels, LON labels on X, LAT labels on Y
  endif else begin
    xtickn = lontickname[gdlon]
    xtickv = xlon[gdlon]
    ytickn = lattickname[gdlat]
    ytickv = ylat[gdlat]

    if keyword_set(nolonticklevels) then xtickf='(A1)'
    if keyword_set(nolatticklevels) then ytickf='(A1)'

    if not keyword_set(xtit) then xtit=lontitle
    if not keyword_set(ytit) then ytit=lattitle

    if keyword_set(lonlabelsflip) then xaxis=1
    if keyword_set(latlabelsflip) then yaxis=1
  endelse
  xticks = n_elements(xtickn)-1
  yticks = n_elements(ytickn)-1

  ; X/Y ranges
  xr = [miny,maxy]
  if keyword_set(xflip) then xr=reverse(xr)
  yr = [minx,maxx]
  if keyword_set(yflip) then yr=reverse(yr)

  ;plot,dist(5),/nodata,/noerase,xtickname=ytickn,xtickv=ytickv,ytickname=xtickn,$
  ;     ytickv=xtickv,position=position,xminor=1,yminor=1,xticklen=0.0001,yticklen=0.0001,$
  ;     xticks=yticks,yticks=xticks,xr=xr,yr=yr,xs=1,ys=1,xtit=ytit,ytit=xtit,$
  ;     xtickformat=ytickf,ytickformat=xtickf,color=framecolor,charsize=charsize

  ; Setup coordinate system
  plot,[0],[0],/nodata,/noerase,xs=1,ys=1,xr=xr,yr=yr,position=position,xminor=1,yminor=1,$
       xtickformat='(A1)',ytickformat='(A1)',xticks=1,yticks=1

  ; Use axis to plot the annotations
  AXIS,xaxis=xaxis,xtickname=ytickn,xtickv=ytickv,xminor=1,xticklen=0.0001,xticks=yticks,$
       xtit=ytit,xtickformat=ytickf,color=framecolor,charsize=charsize,xs=1
  AXIS,yaxis=yaxis,ytickname=xtickn,ytickv=xtickv,yminor=1,yticklen=0.0001,yticks=xticks,$
       ytit=xtit,ytickformat=xtickf,color=framecolor,charsize=charsize,ys=1

  ; Overplot the grid lines
  for i=0,nlon-1 do begin
    x1 = reform(xlon_arr[i,*])
    y1 = reform(ylon_arr[i,*])
    gdpts = where(x1 ge minx and x1 le maxx and y1 ge miny and y1 le maxy,ngdpts)
    if ngdpts gt 0 then $
      oplot,[y1[gdpts]],[x1[gdpts]],color=color,thick=thick,linestyle=linestyle
  end
  for i=0,nlat-1 do begin
    x1 = reform(xlat_arr[i,*])
    y1 = reform(ylat_arr[i,*])
    gdpts = where(x1 ge minx and x1 le maxx and y1 ge miny and y1 le maxy,ngdpts)
    if ngdpts gt 0 then $
      oplot,[y1[gdpts]],[x1[gdpts]],color=color,thick=thick,linestyle=linestyle
  end

  ;; Overplot the grid
  ;for i=0,nlon-1 do oplot,ylon_arr[i,*],xlon_arr[i,*],color=color,thick=thick,linestyle=linestyle
  ;for i=0,nlat-1 do oplot,ylat_arr[i,*],xlat_arr[i,*],color=color,thick=thick,linestyle=linestyle

endelse


if keyword_set(stp) then stop

end
