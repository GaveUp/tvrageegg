proc monthNameToNumber { monthName } {
   return [string map {Jan 01 Feb 02 Mar 03 Apr 04 May 05 Jun 06 Jul 07 Aug 08 Sep 09 Oct 10 Nov 11 Dec 12} $monthName]
}

proc clockarith { seconds delta units } {
    set stamp [clock format $seconds -format "%Y%m%dT%H%M%S"]
     if { $delta < 0 } {
         append stamp " " - [expr { - $delta }] " " $units
     } else {
         append stamp "+ " $delta " " $units
     }
     return [clock scan $stamp]
 }

 proc difftimes { s1 s2 } {

     # Arithmetic has to be done left to right here!

     # Calculate the offset of years.

     set y1 [clock format $s1 -format %Y]
     set y2 [clock format $s2 -format %Y]
     set y [expr { $y1 - $y2 - 1 }]

     set s2new $s2
     set yOut $y

     set s [clockarith $s2 $y years]
     while { $s <= $s1 } {
         set s2new $s
         set yOut $y
         incr y
         set s [clockarith $s2 $y years]
     }
     set s2 $s2new

     # OK, now we know that s2 <= s1.  It's easiest to do months
     # just by counting from 0.

     set m 0
     set mOut 0
     set s [clockarith $s2 $m months]
     while { $s <= $s1 } {
         set s2new $s
         set mOut $m
         incr m
         set s [clockarith $s2 $m months]
     }
     set s2 $s2new

     # s2 is still <= s1, now do days.

     set d [expr { ( ( $s2 - $s1 ) / 86400 ) - 1 }]
     set dOut $d
     set s [clockarith $s2 $d days]
     while { $s <= $s1 } {
         set s2new $s
         set dOut $d
         incr d
         set s [clockarith $s2 $d days]
     }
     set s2 $s2new

     # Hours

     set hh [expr { ( ( $s2 - $s1 ) / 3600 ) - 1 }]
     set hhOut $hh
     set s [clockarith $s2 $hh hours]
     while { $s <= $s1 } {
         set s2new $s
         set hhOut $hh
         incr hh
         set s [clockarith $s2 $hh hours]
     }
     set s2 $s2new

     # Minutes

     set mm [expr { ( ( $s2 - $s1 ) / 60 ) - 1 }]
     set mmOut $hh
     set s [clockarith $s2 $mm minutes]
     while { $s <= $s1 } {
         set s2new $s
         set mmOut $mm
         incr mm
         set s [clockarith $s2 $mm minutes]
     }
     set s2 $s2new

     # Seconds

     set ssOut [expr { $s1 - $s2 }]

     return [list $yOut $mOut $dOut $hhOut $mmOut $ssOut]

 }

proc calculateTimeUntil { t } {
   set t2 [clock scan now]
   set t1 [clock scan [monthNameToNumber $t]]
   set out ""
   if { $t1 < $t2 } {
      foreach f {years months days hours minutes seconds} v [difftimes $t2 $t1] {
         if {$v > 0} {
            append out $v [string map {years y months m days d hours h minutes m seconds s} $f] " "
         }
      }
   
      append out "ago"
   } else {
      foreach f {years months days hours minutes seconds} v [difftimes $t1 $t2] {
         if {$v > 0} {
            append out $v [string map {years y months m days d hours h minutes m seconds s} $f] " "
         }
      }
   }

   return $out
}

#puts [calculateTimeUntil "Nov/11/2009"]
#puts [calculateTimeUntil "Nov/11/2010"]
