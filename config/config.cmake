##... CMake Message Color Declaration
if(NOT WIN32)
  string(ASCII 27 Esc)
  set(ColourReset "${Esc}[m")
  set(ColourBold  "${Esc}[1m")
  set(Red         "${Esc}[31m")
  set(Green       "${Esc}[32m")
  set(Yellow      "${Esc}[33m")
  set(Blue        "${Esc}[34m")
  set(Magenta     "${Esc}[35m")
  set(Cyan        "${Esc}[36m")
  set(White       "${Esc}[37m")
  set(BoldRed     "${Esc}[1;31m")
  set(BoldGreen   "${Esc}[1;32m")
  set(BoldYellow  "${Esc}[1;33m")
  set(BoldBlue    "${Esc}[1;34m")
  set(BoldMagenta "${Esc}[1;35m")
  set(BoldCyan    "${Esc}[1;36m")
  set(BoldWhite   "${Esc}[1;37m")
else()
  set(ColorReset  "")
  set(ColorBold   "")
  set(Red         "")
  set(Green       "")
  set(Yellow      "")
  set(Blue        "")
  set(Magenta     "")
  set(Cyan        "")
  set(White       "")
  set(BoldRed     "")
  set(BoldGreen   "")
  set(BoldYellow  "")
  set(BoldBlue    "")
  set(BoldMagenta "")
  set(BoldCyan    "")
  set(BoldWhite   "")
endif()

##... Installation Path
set( PROJECT_COMPILE_PATH ${CMAKE_HOME_DIRECTORY} )
##... Executable Output Path
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_COMPILE_PATH}/bin )
##... Library Output Path
set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_COMPILE_PATH}/lib )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_COMPILE_PATH}/lib )
##... Fortran Module Header File Output Path
set( CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_COMPILE_PATH}/lib )

## Git ---------------------------------------------------------------------- ##

#... Get current branch name
execute_process(
  COMMAND               git rev-parse --abbrev-ref HEAD
  WORKING_DIRECTORY     ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE       GIT_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

#... Get current commit hash
execute_process(
  COMMAND               git log -1 --format=%H
  WORKING_DIRECTORY     ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE       GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

#... Add Preprocessor for program
add_definitions("-D__GIT_BRANCH__=\"${GIT_BRANCH}\"")
add_definitions("-D__GIT_COMMIT_HASH__=\"${GIT_COMMIT_HASH}\"")

## OS Environment ----------------------------------------------------------- ##

if( WIN32 )
    add_definitions(-D__OS_WINDOWS__)
elseif( UNIX )
    add_definitions(-D__OS_LINUX__)
endif()
