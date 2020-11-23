;+
;
; LOADCOL
;
; This program loads my own personal color tables
;
; 1 - Purple, blue, green, yellow, orange, red (made with xloadct, started
;      with color table 39 and changed the upper/lower boundaries with in the
;      plot under the function table).
; 2 - Same as "1" but with color index=1 set to White
; 3 - White to Black.  Reverse of color table 0
; 4 - Same as "3", but a better upper/lower boundaries (color index 0=White, 255=Black)
; 5 - Same as "4", but with color index=1 set to White
; 6 - Same as "5", but with a stretch (gamma = 1.98) that high-lights the darker colors
; 7 - White to Black, scaled so the top is gray
; 8 - Blue, green, yellow, orange red. 0=blue, 255=white
; 9 - White to Black, scaled so the top is a dark gray, 255=black
; 10 - Red temperature with less dark cutoff (bright to dark).
; 11 - Gray, yellow, orange, red (from K. Barger).
; 12 - Rainbow (color table 39) but gradient to white at low end.
;
; By D.Nidever  Mayb 2007
;-
pro loadcol,index,list=list,reverse=reverse

; Listing the color tables
if keyword_set(list) then begin
  print,'Available color tables:'
  print,' 1 - Purple, blue, green, yellow, orange, red (made with xloadct, started'
  print,'      with color table 39 and changed the upper/lower boundaries with in the'
  print,'      plot under the function table).'
  print,' 2 - Same as "1" but with color index=1 set to White'
  print,' 3 - White to Black.  Reverse of color table 0'
  print,' 4 - Same as "3", but a better upper/lower boundaries (color index 0=White, 255=Black)'
  print,' 5 - Same as "4", but with color index=1 set to White'
  print,' 6 - Same as "5", but with a stretch (gamma = 1.98) that high-lights the darker colors'
  print,' 7 - White to Black, scaled so the top is gray'
  print,' 8 - Blue, green, yellow, orange red. 0=blue, 255=white'
  print,' 9 - White to Black, scaled so the top is a dark gray, 255=black'
  print,'10 - Red temperature with less dark cutoff (bright to dark)'
  print,'11 - Gray, yellow, orange, red (from K. Barger)'
  print,'12 -  Rainbow (color table 39) but gradient to white at low end.'
  return
endif

; Not enough inputs
if n_elements(index) eq 0 then begin
  print,'Syntax - loadcol,index,list=list'
  return
endif

;dir = userdir()
;dir = '/net/home/dln5q/'
dir = '/Users/nidever/'
fdir = dir+'/idl/'
name = 'coltable'+strtrim(index,2)+'.dat'
file = fdir+name

test = file_test(file)
if test eq 0 then begin
  print,'NO COLOR TABLE ',index
  return
endif
restore,file

if keyword_set(reverse) then tvlct,reverse(r),reverse(g),reverse(b) else $
  tvlct,r,g,b

;stop

end
