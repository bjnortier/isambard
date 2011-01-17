cmake_minimum_required (VERSION 2.6)
project (Worker)

# The version number.
set (Tutorial_VERSION_MAJOR 0)
set (Tutorial_VERSION_MINOR 1)

# configure a header file to pass some of the CMake settings
# to the source code
configure_file (
  "${PROJECT_SOURCE_DIR}/TutorialConfig.h.in"
  "${PROJECT_BINARY_DIR}/TutorialConfig.h"
  )

# add the binary tree to the search path for include files
# so that we will find TutorialConfig.h
include_directories("${PROJECT_BINARY_DIR}")
 
# add the executable
add_executable(Tutorial tutorial.cxx)

include_directories ("/Library/OpenCASCADE/6.3.0/inc")

set(CMAKE_CXX_FLAGS "-DHAVE_CONFIG_H")

target_link_libraries(Tutorial -lpthread -L/Library/OpenCASCADE/6.3.0/lib -lTKernel -lPTKernel -lTKMath -lTKService -lTKV3d -lTKV2d -lTKBRep -lTKIGES -lTKSTL -lTKVRML -lTKSTEP -lTKSTEPAttr -lTKSTEP209 -lTKSTEPBase -lTKShapeSchema -lTKGeomBase -lTKGeomAlgo -lTKG3d -lTKG2d -lTKXSBase -lTKPShape -lTKShHealing -lTKHLR -lTKTopAlgo -lTKMesh -lTKPrim -lTKCDF -lTKBool -lTKBO -lTKFillet -lTKOffset)


