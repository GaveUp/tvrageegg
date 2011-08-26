namespace eval tvrage {

if [info exists tvrage] { 
   unset tvrage
}

set tvrage(version) "v2.0b6-dev"
set tvrage(versionLine) "TVRage.com Primetime Schedule Script $tvrage(version)"
set tvrage(scriptPath) [file dirname [info script]]

putlog "Loading $tvrage(versionLine)"

if {[catch {package require http} error]} {
   putlog "tvrage.tcl error: egghttp required."
   return
}

if {$tcl_version < "8.5"} {
   if {[catch {package require dict} error]} {
      putlog "tvrage.tcl error: dict required."
      return
   }
}

if {[catch {source $tvrage(scriptPath)/includes/tvrage.datediff.tcl} error]} {
   putlog "tvrage.tcl error: Unable to load tvrage.datediff.tcl ($error), cannot continue."
   return
}

if {[catch {source $tvrage(scriptPath)/tvrage.conf.defaults} error]} {
   putlog "tvrage.tcl error: Unable to load tvrage.conf.defaults ($error), cannot continue."
   return
}

if {[catch {source $tvrage(scriptPath)/themes/default.theme} error]} {
   putlog "tvrage.tcl error: Unable to load themes/default.theme ($error), cannot continue."
   return
}

if {[catch {source $tvrage(scriptPath)/tvrage.conf} error]} {
   putlog "tvrage.tcl warning: Unable to load tvrage.conf.  Using default values."
} else {
   putlog "tvrage.tcl info: Loaded tvrage.conf successfully."
}

if {[catch {source $tvrage(scriptPath)/themes/$tvrage(theme)} error]} {
   putlog "tvrage.tcl warning: Unable to load themes/$tvrage(theme)."
} else {
   putlog "tvrage.tcl info: Loaded themes/$tvrage(theme) successfully."
}

bind time - "00 * * * *" [namespace current]::updateCache

if {$tvrage(enableAnnounce)} {
   bind time - "?? * * * *" [namespace current]::announceShows
}

if {$tvrage(pubTriggers)} {
   set tvrage(triggerType) "pub"
} else {
   set tvrage(triggerType) "msg"
}

if {$tvrage(enableSchedule)} {
   foreach trigger [split $tvrage(todayTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(todayFlags) $trigger [namespace current]::today
   }
   foreach trigger [split $tvrage(tomorrowTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(tomorrowFlags) $trigger [namespace current]::tomorrow
   }
   foreach trigger [split $tvrage(scheduleTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(scheduleFlags) $trigger [namespace current]::schedule
   }
   foreach trigger [split $tvrage(availableCountriesTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(availableCountriesFlags) $trigger [namespace current]::availableCountries
   }
   foreach trigger [split $tvrage(updateCacheTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(updateCacheFlags) $trigger [namespace current]::updateCache
   }
   foreach trigger [split $tvrage(yesterdayTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(yesterdayFlags) $trigger [namespace current]::yesterday
   }
   foreach trigger [split $tvrage(primetimeTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(primetimeFlags) $trigger [namespace current]::primetime
   }
}
if {$tvrage(enableShowInfo)} {
   foreach trigger [split $tvrage(showinfoTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(showinfoFlags) $trigger [list [namespace current]::showinfo "showInfoLine"]
   }
}
if {$tvrage(enableHelp)} {
   foreach trigger [split $tvrage(helpTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(helpFlags) $trigger [namespace current]::help
   }
}
if {$tvrage(enableSummary)} {
   foreach trigger [split $tvrage(summaryTrigger) " "] {
      bind $tvrage(triggerType) $tvrage(summaryFlags) $trigger [namespace current]::getSummary
   }
}

setudef flag tv
setudef flag tvlog
setudef flag tvannounce

array set schedule {}

proc init {} {
	variable tvrage

	if {[info exists tvrage(tvlogTimer)]} {
		if {[catch { killutimer $tvrage(tvlogTimer) } message] } {
			debug DEBUG $message
		}
	}

	if { $tvrage(enableTVLog) } {
		if { [file exists "$tvrage(tvlog)"] } {
			set tvrage(tvlogSize) [file size $tvrage(tvlog)]
			set tvrage(tvlogTimer) [utimer $tvrage(tvlogTimerDelay) "readTVLog"]
		} else {
			debug DEBUG "ERROR: init{}: \"$tvrage(tvlog)\" does not exist"
		}
	}	

   setupCustomTriggers

   updateCache 1 2 3 4 5
}

proc setupCustomTriggers {} {
   variable tvrage

   foreach cTrigger [array names tvrage -regex {^cTrigger}] {
      set cTemplateName [string map {cTrigger cTemplate} $cTrigger]
      set cFlagsName [string map {cTrigger cFlags} $cTrigger]
      if {![info exists tvrage($cTemplateName)]} {
         debug ERROR "WARNING: $cTrigger contains no matching cTemplate.  $cTrigger skipped."
      } else {
         set cFlags ""
         if {![info exists tvrage($cFlagsName)]} {
            debug INFO "INFO: No Flags defined for $cTrigger.  Using showinfoFlags ($tvrage(showinfoFlags))."
            set cFlags $tvrage(showinfoFlags)
         } else {
            set cFlags $tvrage($cFlagsName)
         }

         foreach trigger [split $tvrage($cTrigger) " "] {
            debug INFO "INFO: Adding Custom Trigger: $trigger"
            bind $tvrage(triggerType) $cFlags $trigger [list [namespace current]::showinfo $cTemplateName]
         }
      }
   }
}

proc availableCountries {nick uhost hand chan text} {
   variable tvrage

   if ![channel get $chan tv] { return }

   set show(upperAvailableCountries) [string toupper $tvrage(availableCountries)]
   set show(lowerAvailableCountries) [string tolower $tvrage(availableCountries)]
   set show(availableCountries) $tvrage(availableCountries)
   set show(chan) $chan
   set show(nick) $nick

   displayInfo [templateParser $tvrage(availableCountriesLine)  [array get show]]
}

proc help {nick uhost hand chan text} {
	variable tvrage

	if ![channel get $chan tv] { return }

	set show(nick) $nick
	set show(chan) $chan
	set show(scheduleTrigger) $tvrage(scheduleTrigger)
	set show(showinfoTrigger) $tvrage(showinfoTrigger)
	set show(todayTrigger) $tvrage(todayTrigger)
	set show(tomorrowTrigger) $tvrage(tomorrowTrigger)
	set show(summaryTrigger) $tvrage(summaryTrigger)
	set show(helpTrigger) $tvrage(helpTrigger)
   set show(availableCountriesTrigger) $tvrage(availableCountriesTrigger)
	set show(version) $tvrage(version)
	set show(versionLine) [templateParser $tvrage(versionLine) [array get show]]
   set show(upperAvailableCountries) [string toupper $tvrage(availableCountries)]
   set show(lowerAvailableCountries) [string tolower $tvrage(availableCountries)]
   set show(availableCountries) $tvrage(availableCountries)
   set show(yesterdayTrigger) $tvrage(yesterdayTrigger)
   set show(updateCacheTrigger) $tvrage(updateCacheTrigger)
   set show(enableSchedule) $tvrage(enableSchedule)
   set show(enableShowInfo) $tvrage(enableShowInfo)
   set show(enableSummary) $tvrage(enableSummary)
   set show(primetimeTrigger) $tvrage(primetimeTrigger)

	displayInfo [templateParser $tvrage(help) [array get show]]

}

proc readTVLog {} {
	variable tvrage
	
	set tvrage(tvlogTimer) [utimer $tvrage(tvlogTimerDelay) "readTVLog"]
	set curSize [file size $tvrage(tvlog)]
	if { $curSize == $tvrage(tvlogSize) } { return 0 }
	if { $curSize < $tvrage(tvlogSize) } {
		set tv(tvlogSize) $curSize
		debug DEBUG "INFO: readTVLogP{}: $tvrage(tvlog) smaller than last readTVLog{}."
	}
	if { [catch { set fh [open $tvrage(tvlog) r] }] } {
		debug DEBUG "ERROR: readTVLog{}: Could not open $tvrage(tvlog) for reading."
		return 0
	}

	seek $fh $tvrage(tvlogSize)
	while { ![eof $fh] } {
		if { [catch { set line [string trimright [gets $fh]] }] } {
			debug DEBUG "ERROR: readTVLog{}: Could not read from $tvrage(tvlog)."
			break
		}
		if { [string index $line 0] == "#" } { continue }
		if { [llength $line] == 0 } { continue }

		if { [lindex [split $line " "] 5] == "NEWDIR:" } {
			set info [exec basename [lindex [split $line " "] 6]]
			set info [string trimright $info "\""]
			getInfo $info
		}
	}

	close $fh
	set tvrage(tvlogSize) [file size $tvrage(tvlog)]
	return 0
}

proc getInfo {info} {
	variable tvrage

	set show(dirname) $info
	set pattern1 {^([\w\d\.\_\-]+)\.S([\d]{1,2})E([\d]{1,2})}
	set pattern2 {^([\w\d\.\_\-]+)\.([\d]{1,2})x([\d]{1,2})}

	if { ![regexp "$pattern1" $info => title season episode] && ![regexp "$pattern2" $info => title season episode] } {
		debug DEBUG "ERROR: getInfo{}: Unknown Formatting of $info."
		return
	}

	regsub -all -- {\.} $title " " title
	regsub -all -- {\_} $title " " title
	set sxep [convertToSxEp $season $episode]
	
	if [catch {getEpisodeInfo $title $sxep} problem] {
		debug ERROR "TVRage: ERROR: $problem"
		set show(problem) $problem
	}

   foreach chan [channels] {
		if {[channel get $chan tvlog] && [botonchan $chan]} {
			set show(chan) $chan
			if { !$show(found) } {
				displayInfo [templateParser $tvrage(tvlogNoFoundLine) [array get show]]
			} else {
				displayInfo [templateParser $tvrage(tvlogLine) [array get show]]
			}
		}
	}
}

proc convertToSxEp {season episode} {
	set season [string trimleft $season "0"]
	set episode [string trimleft $episode "0"]

	if { $episode < 10 } {
		set episode "0$episode"
	}
	
	return [join [list $season "x" $episode] ""]
}

proc debug {level lines} {
	variable tvrage

   set l 0
   switch $level {
      INFO { set l 1 }
      TEMPLATE { set l 2 }
      DEBUG { set l 3 }
      default {}
   }

   set dl 0
   switch $tvrage(debug) {
      NONE { set dl 0 }
      INFO { set dl 1 }
      TEMPLATE { set dl 2 }
      DEBUG { set dl 3 }
      default {}
   }

   if {$l <= $dl} {
      foreach line [split $lines "\n"] {
         putlog "TVRage: $line"
      }
   }
}

proc wordwrap {str {len 200} {prefix {}} {splitChr { }}} {
	set out {}
	set cur $prefix
	set i 0
	foreach word [split [set str][unset str] $splitChr] {
		if {[incr i [string len $word]]>$len} {
			lappend out [join $cur $splitChr]
         set out [string map {" : " " :"} $out]
			if {[regexp {^.*(\003\d\d)} $cur -> lastColor]} {
				set cur [join [list $prefix $lastColor $word] ""]
			} else {
				set cur [join [list $prefix $word] ""]
			}
			set i [string len $word]
		} {
			lappend cur $word
		}
		incr i
	}
	lappend out [join $cur $splitChr]

   regsub { : } $out { :} out
   return $out
}

proc displayInfo {text {prefix {}} {tosplit {}}} {
	variable tvrage
	
	if {([string len $prefix] == 0) && ([string len $tosplit] == 0)} {
		regexp {^(.*?:)(.*)$} $text -> prefix tosplit
	} elseif {[string len $prefix] == 0} {
		regexp {^(.*?:)(.*)$} $text -> prefix ->
	} elseif {[string len $tosplit] == 0} {
		regexp {^(.*?:)(.*)$} $text -> -> tosplit
	}
	
   foreach inputLine [split $tosplit "\n"] {
      foreach line [wordwrap $inputLine $tvrage(nChars) $prefix] {
         regsub -all {\x92} $line {'} line
         regsub -all {\x2019} $line {'} line
	   	putserv $line
	   }
   }
}

proc getEpisodeInfo {showname ep} {
	variable tvrage
	upvar show show
	set show(found) 0
	set url $tvrage(showinfourl)[http::formatQuery {show} $showname {ep} $ep]
   if {[catch {set token [http::geturl $url -timeout [expr $tvrage(httpTimeout) * 1000]]} error]} {
		error $error
   }

	if { [http::status $token] == "timeout" } {
      error "Timeout retrieving show info."
	} elseif { [http::status $token] != "ok" } {
		set problem [http::error $token]
		[http::cleanup $token]
		error $problem
	}
   set data [http::data $token]
   http::cleanup $token

   foreach line [split $data \n] {
		if {[regexp {^No Show Results Were Found For \"(.*)\"$} $line -> show(title)]} {
			set show(found) 0
			return
      } elseif {[regexp {^Episode Info@(\d+)x(\d+)\^(.*)\^([\w\/]+)$} $line -> season episode eptitle epDate]} { 
			set show(latest) 1
			set show(epTitle) $eptitle
			set show(epSeason) $season
			set show(epNumber) $episode
			set show(epDate) $epDate
			set show(seasonEpisodeSeparator) $tvrage(seasonEpisodeSeparator)
		} else {
         if {[regexp {^(.*)@(.*)$} $line -> trait tValue]} { 
            set trait [string map {{ } {_}} $trait]
            set show([string tolower $trait]) $tValue 
         }
      }
	}

	if {[catch {set token [http::geturl $show(episode_url) -timeout [expr $tvrage(httpTimeout) * 1000]]} error]} {
		error $error
   }
	set data [http::data $token]
	http::cleanup $token

	regsub -all "\n" $data "" data
	regsub -all "<br>" $data "" data
	regsub -all "<\/?b>" $data "" data
	regsub -all {Source\:.*?</div>} $data "</div>" data

	set match {}
	if {[regexp {<div class='show_synopsis'>(.*?)</div>} $data -> match]} {
		set match [string trim $match]
	}
	
	if {[string length $match] > 0 || [regexp "</script></div><div>(.*?)</div>" $data -> match]} { 
      regsub -all "<script .*?</script>" $match "" match
      regsub -all "<a .*?>" $match "" match
      regsub -all "</a>" $match "" match
      regsub -all "\\\[x\\\] Remove Ad" $match "" match
      regsub -all "<b>.*</b>" $match "" match
		regsub -all "<span class='left'></span><span class=\"addthis_toolbox addthis_default_style \">.*$" $match "" match
		set match [string trim $match]
      set show(summary) $match 
   }	

   if {![info exists show(summary)]} { set show(summary) $tvrage(noSummaryAvailable) }
   
	set show(found) 1
}

proc getSummary {nick uhost hand chan text} {
	variable tvrage

	if ![channel get $chan tv] return

   set show(chan) $chan
   set show(nick) $nick
	set show(request) $text
   
	if {![regexp {(.*) (\d+x\d+)} $text -> showname ep]} {
		displayInfo [templateParser $tvrage(summaryInvalidFormatLine) [array get show]]
		return
	}
	
	if [catch {getEpisodeInfo $showname $ep} problem] {
		debug ERROR "TVRage: ERROR: $problem"
		set show(problem) $problem
	}
  
   if {$show(found)} {
		displayInfo [templateParser $tvrage(summaryLine) [array get show]]
	} else {
		displayInfo [templateParser $tvrage(noShowLine) [array get show]]
	}
}

proc templateParser {template info} {
	set filled $template
   array set show $info

   while {[regexp "(\{\@\%(.*?):(.*?):(.*?)\%\@\})" $filled -> fpattern key value str]} {
      if {[string toupper $show($key)] == [string toupper $value]} { 
         set filled [string map [list "$fpattern" "$str"] $filled]
      } else {
         set filled [string map [list "$fpattern" ""] $filled]
      }
   }

   while {[regexp "(\{\e\%(.*?):(.*?)\%\e\})" $filled -> fpattern key str]} {
      if {[info exists show($key)]} {
         if {[string length $show($key)] > 0} {
            set filled [string map [list "$fpattern" "$str"] $filled]
         } else {
            set filled [string map [list "$fpattern" ""] $filled]
         }
      } else {
         set filled [string map [list "$fpattern" ""] $filled]
      }
   }

	foreach {key value} $info {
      debug TEMPLATE "{%%$key%%}"
      regsub -all {\&} $value {\\&} value
		regsub -all "\{\%\%$key\%\%\}" $filled $value filled
	}

	return $filled
}

proc verifyCountry {text} {
   variable tvrage

   foreach co [split $tvrage(availableCountries) { }] {
      if {[string tolower $text] == [string tolower $co]} {
         return 1
      }
   }

   return 0
}

proc schedule {nick uhost hand chan text} {
   variable tvrage

   if ![channel get $chan tv] return
      
	set seconds [clock seconds]
	set days(sun) 0
	set days(mon) 1
	set days(tue) 2
	set days(wed) 3
	set days(thu) 4
	set days(fri) 5
	set days(sat) 6

   set show(nick) $nick
   set show(chan) $chan
   set desiredDay [lindex $text 0]
   set show(country) [lindex $text 1]

   if {[string length $show(country)] == 0} { set show(country) $tvrage(defaultCountry) }
   
   if ![verifyCountry $show(country)] { 
	   displayInfo [templateParser $tvrage(invalidCountry) [array get show]]
      return
   }

	if {[string is integer $desiredDay ]} {
      if {$desiredDay <= 6 && $desiredDay >= [expr (-1 * $tvrage(cacheForDays))]} {
         printSchedule $show(country) $desiredDay $chan $nick
      }
   } else {
      if ![info exist days([string tolower $desiredDay])] return 

	   set currDay [clock format $seconds -format "%w"]
	
      if {$currDay > $days([string tolower $desiredDay])} {
         printSchedule $show(country) [expr (7 - $currDay) + $days([string tolower $text])] $chan $nick
   	} else {
         printSchedule $show(country) [expr $days([string tolower $desiredDay]) - $currDay] $chan $nick
   	}
   }
}

proc yesterday {nick uhost hand chan text} {
   variable tvrage 
   variable schedule
      
	if ![channel get $chan tv] return
   
   set show(nick) $nick
   set show(chan) $chan

   if {![parseCountry $text show(country)]} {
      displayInfo [templateParser $tvrage(invalidCountry) [array get show]]
      return
   }

   printSchedule $show(country) "-1" $chan $nick
}

proc today {nick uhost hand chan text} {
   variable tvrage 
   variable schedule

	if ![channel get $chan tv] return
   
   set show(nick) $nick
   set show(chan) $chan

   if {![parseCountry $text show(country)]} {
      displayInfo [templateParser $tvrage(invalidCountry) [array get show]]
      return
   }

   printSchedule $show(country) "0" $chan $nick
}

proc primetime {nick uhost hand chan text} {
   variable tvrage
   variable schedule

   if ![channel get $chan tv] return

   set show(nick) $nick
   set show(chan) $chan

	set today [clock format [clock seconds] -format "%w"]
	set days(sun) 0
	set days(mon) 1
	set days(tue) 2
	set days(wed) 3
	set days(thu) 4
	set days(fri) 5
	set days(sat) 6
   
   set w 0
   set c $tvrage(defaultCountry)
   set n 0

   foreach arg [split $text] {
      if {[string is integer $arg]} {
         set w $arg
      } else {
         switch $arg {
            today { set w 0 }
            tomorrow { set w 1 }
            yesterday { set w -1 }
            mon -
            tue -
            wed -
            thu -
            fri -
            sat -
            sun {
               if {$today > $days([string tolower $arg])} {
                  set w [expr (7 - $today) + $days([string tolower $text])]
               } else {
                  set w [expr $days([string tolower $arg]) - $today]
               }
            }
            default {
               set f 0
               foreach co [split $tvrage(availableCountries)] {
                  if {[string toupper $arg] == [string toupper $co]} { 
                     set c $arg 
                     set f 1
                  }
               }
               
               if {!$f} { 
                  displayInfo [templateParser $tvrage(invalidPrimetime) [array get show]] 
                  return
               }
            }
         }
      }

      incr n
      if {$n > 2} {
         displayInfo [templateParser $tvrage(invalidPrimetime) [array get show]]
         return
      }
   }

   printSchedule $c $w $chan $nick $tvrage(primetimeStart) $tvrage(primetimeEnd)
}

proc parseCountry {text c} {
   variable tvrage
   upvar $c country
   set country [lindex $text 0]

   if {[string length $country] == 0} { set country $tvrage(defaultCountry) }
   
   return [verifyCountry $country]
}

proc tomorrow {nick uhost hand chan text} {
	variable tvrage 
   variable schedule
      
   if ![channel get $chan tv] return
      
   set show(nick) $nick
   set show(chan) $chan
   
   if {![parseCountry $text show(country)]} { 
	   displayInfo [templateParser $tvrage(invalidCountry) [array get show]]
      return
   }
   
   printSchedule $show(country) "1" $chan $nick
}

proc getShowInfo {displayLine nick chan text} {
	variable tvrage
	variable request
	upvar show show
	set url $tvrage(showinfourl)[http::formatQuery {show} [string trimleft $text]]
	if {[catch {set token [http::geturl $url -command [namespace current]::getShowInfoHandler -timeout [expr $tvrage(httpTimeout) * 1000]]} error]} {
      error $error
   } else {
		set request($token,nick) $nick
		set request($token,chan) $chan
		set request($token,displayLine) $displayLine
	}
}

proc cleanupRequest {token} {
	variable request
	unset request($token,nick)
	unset request($token,chan)
	unset request($token,displayLine)
}

proc getShowInfoHandler {token} {
	variable tvrage
	variable request

	set show(chan) $request($token,chan)
	set show(nick) $request($token,nick)
	set displayLine $request($token,displayLine)

	if { [http::status $token] == "timeout" } {
		set problem "Timeout retrieving show info."
		debug ERROR "TVRage: ERROR: $problem"
		set show(problem) $problem
		displayInfo [templateParser $tvrage(problemMessage) [array get show]]
		cleanupRequest $token
		return
	} elseif { [http::status $token] != "ok" } {
		set problem [http::error $token]
		[http::cleanup $token]
		debug ERROR "TVRage: ERROR: $problem"
		set show(problem) $problem
		displayInfo [templateParser $tvrage(problemMessage) [array get show]]
		cleanupRequest $token
		return
	}

	set data [http::data $token]
	http::cleanup $token

	foreach line [split $data \n] {
		regsub -all {\x92} $line {'} line
		if {[regexp {^No Show Results Were Found For \"(.*)\"$} $line -> show(show_name)]} {
			set show(found) 0
			return show
		} elseif {[regexp {^Latest Episode@(\d+)x(\d+)\^(.*)\^([\w\/]+)$} $line -> season episode eptitle epDate]} { 
			set show(latest) 1
			set show(latestTitle) $eptitle
			set show(latestSeason) $season
			set show(latestEpisode) $episode
			set show(latestDate) $epDate
			set show(latestSeparator) $tvrage(seasonEpisodeSeparator)
		} elseif {[regexp {^Next Episode@(\d+)x(\d+)\^(.*)\^([\w\/]+)$} $line -> season episode eptitle epDate]} { 
			set show(next) 1
			set show(nextTitle) $eptitle
			set show(nextSeason) $season
			set show(nextEpisode) $episode
			set show(nextDate) $epDate
			set show(nextSeparator) $tvrage(seasonEpisodeSeparator)
		} else {
			if {[regexp {^(.*)@(.*)$} $line -> trait tValue]} { 
				set trait [string map {{ } {_}} $trait]
				set show([string tolower $trait]) $tValue 
			}
		}
	}

	if ![info exist show(latest)] { 
		set show(latest) 0
		set show(latestTitle) ""
		set show(latestSeason) ""
		set show(latestEpisode) "N/A"
		set show(latestDate) ""
		set show(latestSeparator) ""
	} else {
		if ![info exist show(airtime)] {
			set show(latestTimeUntil) [calculateTimeUntil [concat $show(latestDate)  "12:00 am"]]
		} else {
			if [catch {set show(latestTimeUntil) [calculateTimeUntil [concat $show(latestDate)  [concat [expr [string trimleft [lindex [split [lindex [split $show(airtime)] 2] {:}] 0] 0] - $tvrage(offsetHours)] ":" [lindex [split [lindex [split $show(airtime)] 2] {:}] 1]] [lindex [split $show(airtime)] 3]]]} error] {
				set show(latestTimeUntil) "Calculation Failed."
			}
		}
	}

	if ![info exist show(next)] { 
		set show(next) 0
		set show(nextTitle) ""
		set show(nextSeason) ""
		set show(nextEpisode) "N/A"
		set show(nextDate) ""
		set show(nextSeparator) ""
		set show(nextTimeUntil) ""
	} else {
		if ![info exist show(airtime)] {
			set show(nextTimeUntil) [calculateTimeUntil [concat $show(nextDate)  "12:00 am"]]
		} else {
			if [catch {set show(nextTimeUntil) [calculateTimeUntil [concat $show(nextDate)  [concat [expr [string trimleft [lindex [split [lindex [split $show(airtime)] 2] {:}] 0] 0] - $tvrage(offsetHours)] ":" [lindex [split [lindex [split $show(airtime)] 2] {:}] 1]] [lindex [split $show(airtime)] 3]]]} error] {
				set show(nextTimeUntil) "Calculation Failed."
			}
		}
	}

	set show(found) 1

	set show(seasonEpisodeSeparator) $tvrage(seasonEpisodeSeparator)

	if {!$show(found)} {
		displayInfo [templateParser $tvrage(noShowLine) [array get show]]
		return
	}
	
	if {$show(next)} {
		set show(next) [templateParser $tvrage(nextEpFormat) [array get show]]
	} else {
		set show(next) [templateParser $tvrage(nextEpNoExistFormat) [array get show]]
	}

	if {$show(latest)} {
		set show(latest) [templateParser $tvrage(latestEpFormat) [array get show]]
	} else {
		set show(latest) [templateParser $tvrage(latestEpNoExistFormat) [array get show]]
	}

	displayInfo [templateParser $tvrage($displayLine) [array get show]]

	cleanupRequest $token
}

proc showinfo {displayLine nick uhost hand chan text} {
	variable tvrage

   if ![channel get $chan tv] return

	if [catch {getShowInfo $displayLine $nick $chan $text} problem] {
		debug ERROR "TVRage: ERROR: $problem"
		set show(problem) $problem
		displayInfo [templateParser $tvrage(problemMessage) [array get show]]
	}
}

proc announceShows {minute hour day month year} {
   variable tvrage
   variable schedule

   set time [expr [clock seconds] + $tvrage(minutesBefore) * 60 + $tvrage(offsetHours) * 3600]

   set cDate [clock format [expr $time - $tvrage(fudgeMinutes) * 60] -format "%A, %d %b %Y"]
   set cTime [string tolower [clock format $time -format "%I:%M %p"]]
   set data(announceShowSeparator) $tvrage(announceShowSeparator)
   set data(minutesBefore) $tvrage(minutesBefore)

   if {[dict exists $schedule($tvrage(announceCountry):schedule) $cDate $cTime]} {
      set cLine {}

      foreach s [dict get $schedule($tvrage(announceCountry):schedule) $cDate $cTime] {
         foreach k [dict keys $s] {
            set data($k) [dict get $s $k]
         }

         if {$tvrage(includeOnlyFiltered)} {
            set include 0
            foreach n $tvrage(filteredNetworks) {
               if {$n == $data(network)} {
                  set include 1
               }
            }
         } else {
            set include 1
            foreach n $tvrage(filteredNetworks) {
               if {$n == $data(network)} {
                  set include 0
               }
            }
         }

         if {$include} {
				append cLine [string trimleft [templateParser $tvrage(announceShowsFormat) [array get data]]]
         }
      }

      if {[string length $cLine] > 0} {
			if {[string length $tvrage(announceShowSeparator)] > 0} {
				set cLine [string replace $cLine [expr [string length $cLine] - [string length $tvrage(announceShowSeparator)] - 1] [string length $cLine]]
			}
			set data(shows) [string trim $cLine $tvrage(announceShowSeparator)]
         foreach data(chan) [channels] {
            if {[botonchan $data(chan)] && [channel get $data(chan) tvannounce]} {
      	   	displayInfo [templateParser $tvrage(announceLine) [array get data]]
            }
         }
      }
   }
}

proc updateCache {m h d mo y} {
   variable tvrage 
   variable schedule
   set now [clock scan [clock format [clock seconds] -format "%m/%d/%y"]]
   set old [expr ($now - ($tvrage(cacheForDays) * 86400))]

   foreach c [split $tvrage(availableCountries)] {
      if {[info exists schedule($c:dates)]} {
         set upd ""

         foreach d $schedule($c:dates) {
            scan $d {%s %d %s %d} name day month year
            set cur [clock scan "$day $month $year"]
            lappend upd $d

            if {($cur < $old) || ($cur >= $now)} {
               set in [lsearch -exact $schedule($c:dates) $d]
               set schedule($c:dates) [lreplace $schedule($c:dates) $in $in]
               set schedule($c:schedule) [dict remove $schedule($c:schedule) $d]
               set schedule($c:times) [dict remove $schedule($c:times) $d]
            }
         }
      }
 
      getSchedule $c
   }
}

proc printSchedule {country when chan nick {startTime {}} {endTime {}}} {
   variable tvrage 
   variable schedule

   set data(date) [calculateDate $when]
   set data(chan) $chan
   set data(nick) $nick
   set country [string toupper $country]
   set data(country) $country
   set data(scheduleShowSeparator) $tvrage(scheduleShowSeparator)

   set sTime {}
   if {[string length $startTime] != 0} {
      set sTime [clock scan $startTime]
   }
   
   set eTime {}
   if {[string length $endTime] != 0} {
      set eTime [clock scan $endTime]
   }

   if {![scheduleCached $country $data(date)]} {
      displayInfo [templateParser $tvrage(notCached) [array get data]]
      return
   } else {
      if {![dict exists $schedule($country:times) $data(date)]} {
         displayInfo [templateParser $tvrage(noNewShows) [array get data]]
         return
      } else {
         displayInfo [templateParser $tvrage(scheduleHeader) [array get data]]

         foreach data(time) [dict get $schedule($country:times) $data(date)] {
            if {[string length $sTime] != 0} {
               if {[clock scan $data(time)] < $sTime} {
                  continue
               }
            }

            if {[string length $eTime] != 0} {
               if {[clock scan $data(time)] > $eTime} {
                  continue
               }
            }
            
            set currentTime [templateParser $tvrage(scheduleTimeFormat) [array get data]]
            set cLine {}
            set cLineNew {}
            foreach s [dict get $schedule($country:schedule) $data(date) $data(time)] {
               foreach k [dict keys $s] {
                  set data($k) [dict get $s $k]
               }
               
               if {$tvrage(includeOnlyFiltered)} {
                  set include 0
                  foreach n $tvrage(filteredNetworks) {
                     if {$n == $data(network)} {
                        set include 1
                     }
                  }
               } else {
                  set include 1
                  foreach n $tvrage(filteredNetworks) {
                     if {$n == $data(network)} {
                        set include 0
                     }
                  }
               }

               if {$include} {
                  append cLine [string trimleft [templateParser $tvrage(scheduleEpisodeFormat) [array get data]]]
               }
            }

            if {[string length $cLine] > 0} {
   				set data(scheduleLine) [concat $currentTime [string trim [string trimright $cLine] $tvrage(scheduleShowSeparator)]]
	   			set outputLine [templateParser $tvrage(scheduleLine) [array get data]]
               set re "^(.*?:$currentTime)(.*)$"
		   		regexp $re $outputLine -> prefix tosplit
			   	displayInfo $outputLine $prefix $tosplit
				   unset prefix
   				unset tosplit
            }
         }
      }
   }
}

proc scheduleCached {country date} {
   variable schedule

   if {[info exists schedule($country:dates)]} {
      foreach d $schedule($country:dates) {
         if {$d == $date} {
            return 1
         }
      }
   }

   return 0
}

proc calculateDate {when} {
   set systemTime [clock seconds]
	set systemTime [expr "$systemTime + ($when * 86400)"]
	set neededDate [clock format $systemTime -format "%A, %d %b %Y"]

   return $neededDate
}

proc getSchedule {country} {
   variable tvrage 
   variable schedule
	set queryurl $tvrage(scheduleurl)[http::formatQuery {country} $country]
   if {[catch {set token [http::geturl $queryurl -timeout [expr $tvrage(httpTimeout) * 1000]]} error]} {
      debug ERROR "TVRage: ERROR: $error"
      return
   }
   set data [http::data $token]
   http::cleanup $token

   if {![info exists schedule($country:dates)]} { set schedule($country:dates) {} }
   if {![info exists schedule($country:times)]} { set schedule($country:times) {} }
   if {![info exists schedule($country:schedule)]} { set schedule($country:schedule) {} }

   foreach line [split $data \n] {
      if {[regexp {\[DAY\](.*)\[\/DAY\]} $line -> date]} {
         set in [lsearch -exact $schedule($country:dates) $date]
         set schedule($country:dates) [lreplace $schedule($country:dates) $in $in]
         set schedule($country:schedule) [dict remove $schedule($country:schedule) $date]
         set schedule($country:times) [dict remove $schedule($country:times) $date]
         lappend schedule($country:dates) $date
         continue
      }
      if {[regexp {^\[TIME\](.*)\[/TIME\]$} $line -> t]} {
         dict lappend schedule($country:times) $date $t
         continue
      }
      if {[regexp {^\[SHOW\]((.*)\^(.*)\^(.*)\^(.*))\[/SHOW\]$} $line -> full network show epnum url]} {
         dict update schedule($country:schedule) $date dayinfo {
            dict lappend dayinfo $t [dict create "network" $network "show" $show "epnum" $epnum "url" $url]
         }
      }
   }
}

init
putlog "Successfully loaded $tvrage(versionLine)"

}
