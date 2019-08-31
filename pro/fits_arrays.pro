pro fits_arrays,head,x,y,z,stp=stp,noprint=noprint,silent=silent,$
                xname = ctype1, yname = ctype2, zname = ctype3

; This program creates the x,y,z arrays for a FITS image

;sz = size(im)
;dim = sz(0)
nhead = n_elements(head)

; Incorrect inputs
if nhead eq 0 then begin
  print,'Syntax - fits_arrays,head,x,y,z,xname=xname,yname=yname,zname=zname'
  return
end

if keyword_set(silent) then noprint=1

dim = sxpar(head,'NAXIS')

; X array
naxis1 = sxpar(head,'NAXIS1',count=n_naxis1)
crval1 = sxpar(head,'CRVAL1',count=n_crval1)
cd1 = sxpar(head,'CDELT1',count=n_cd1)
if n_cd1 eq 0 then cd1 = sxpar(head,'CD1_1',count=n_cd1)
;if !err eq -1 then cd1 = sxpar(head,'CD1_1',count=n_cd1_1)
crpix1 = sxpar(head,'CRPIX1',count=n_crpix1)
ctype1 = sxpar(head,'CTYPE1',count=n_ctype1)

;if naxis1 eq 0 or n_crval1 eq 0 or n_ctype1 eq 0 or $
;   n_cd1 eq 0 then begin
if naxis1 eq 0 or n_crval1 eq 0 or n_cd1 eq 0 then begin
  print,'Coordinate parameters for X dimension not complete'
  return
endif
if n_ctype1 eq 0 then ctype1='UNKNOWN'

x = double(crval1) + double(cd1) * (dindgen(naxis1) + 1.0d0 - double(crpix1))
if not keyword_set(noprint) then $
  print,'X = '+ctype1+'  [X] = ',strtrim(naxis1,2)

;stop

; Y array
undefine,y
if (dim ge 2) then begin
  naxis2 = sxpar(head,'NAXIS2',count=n_naxis2)
  crval2 = sxpar(head,'CRVAL2',count=n_crval2)
  cd2 = sxpar(head,'CDELT2',count=n_cd2)
  ;if !err eq -1 then cd2 = sxpar(head,'CD1_2')
  ;if !err eq -1 then cd2 = sxpar(head,'CD2_2',count=n_cd2_2)
  if n_cd2 eq 0 then cd2 = sxpar(head,'CD2_2',count=n_cd2)
  crpix2 = sxpar(head,'CRPIX2',count=n_crpix2)
  ctype2 = sxpar(head,'CTYPE2',count=n_ctype2)

  if naxis2 eq 0 or n_crval2 eq 0 or n_ctype2 eq 0 or $
    n_cd2 eq 0 then begin
    print,'Coordinate parameters for Y dimension not complete'
    return
  endif

  y = double(crval2) + double(cd2) * (dindgen(naxis2) + 1.0d0 - double(crpix2))
  if not keyword_set(noprint) then $
    print,'Y = '+ctype2+'  [Y] = ',strtrim(naxis2,2)
end


; Z array
undefine,z
if (dim ge 3) then begin
  naxis3 = sxpar(head,'NAXIS3',count=n_naxis3)
  crval3 = sxpar(head,'CRVAL3',count=n_crval3)
  cd3 = sxpar(head,'CDELT3',count=n_cd3)
  ;if !err eq -1 then cd3 = sxpar(head,'CD1_3')
  if n_cd3 eq 0 then cd3 = sxpar(head,'CD3_3',count=n_cd3)
  crpix3 = sxpar(head,'CRPIX3',count=n_crpix3)
  ctype3 = sxpar(head,'CTYPE3',count=n_ctype3)

  if naxis3 eq 0 or n_crval3 eq 0 or n_ctype3 eq 0 or $
    n_cd3 eq 0 then begin
    print,'Coordinate parameters for Z dimension not complete'
    return
  endif

  z = double(crval3) + double(cd3) * (dindgen(naxis3) + 1.0d0 - double(crpix3))
  if not keyword_set(noprint) then $
    print,'Z = '+ctype3+'  [Z] = ',strtrim(naxis3,2)
end

if keyword_set(stp) then stop

end
