If (Test-Path java){
  Remove-Item java -Recurse
}

If (Test-Path mustangproject){
  Remove-Item mustangproject -Recurse
}

#If (Test-Path apachemaven){
#  Remove-Item apachemaven -Recurse
#}

Invoke-WebRequest -Uri "https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.6%2B10/OpenJDK17U-jre_x64_windows_hotspot_17.0.6_10.zip" -OutFile jre.zip
New-Item -Name "mustangproject" -ItemType Directory
Invoke-WebRequest -Uri "https://github.com/ZUGFeRD/mustangproject/releases/download/core-2.11.0/Mustang-CLI-2.11.0.jar" -OutFile mustangproject\Mustang-CLI-2.11.0.jar
#Invoke-WebRequest -Uri "https://dlcdn.apache.org/maven/maven-3/3.9.6/binaries/apache-maven-3.9.6-bin.zip" -OutFile apachemaven.zip

Expand-Archive jre.zip
#Expand-Archive apachemaven.zip
Move-Item .\jre\jdk-17.0.6+10-jre .\java
Remove-Item jre -Recurse

If (Test-Path jre.zip){
  Remove-Item jre.zip
}
#If (Test-Path apachemaven.zip){
#  Remove-Item apachemaven.zip
#}
