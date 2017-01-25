#!/bin/bash
OutFileName="merged.csv"                       # Fix the output name
i=0                                       # Reset a counter
for filename in ./*.csv; do 
 if [ "$filename"  != "$OutName" ] ;      # Avoid recursion 
 then 
   if [[ $i -eq 0 ]] ; then 
      head -1  $filename >   $OutFileName # Copy header if it is the first file
   fi
   tail -n +2  $filename >>  $OutFileName # Append from the 2nd line each file
   i=$(( $i + 1 ))                        # Increase the counter
 fi
done

# Now fix it so if thre is no header then I will just cat the files 
if [[ $1 -eq 1 ]] ; then 
  i=1
  rm -f ${OutFileName}
  for filename in ./*.csv; do 
   if [ "$filename"  != "$OutName" ] ;      # Avoid recursion 
   then 
     if [[ $i -eq 0 ]] ; then 
        head -1  $filename >   $OutFileName # Copy header if it is the first file
     fi
     cat  $filename >>  $OutFileName # Append from the 2nd line each file
     i=$(( $i + 1 ))                        # Increase the counter
   fi
  done
fi
