
// A simple program that computes the square root of a number
#include <stdio.h>
#include <iostream>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>


// OpenCASCADE
#include <BRep_Tool.hxx>
#include <BRepLib.hxx>
#include <BRepMesh.hxx>
#include <BRepPrimAPI_MakeBox.hxx>
#include <BRepPrimAPI_MakeSphere.hxx>
#include <BRepPrimAPI_MakeCylinder.hxx>
#include <BRepPrimAPI_MakeCone.hxx>
#include <BRepPrimAPI_MakeWedge.hxx>
#include <BRepPrimAPI_MakeTorus.hxx>
#include <BRepAlgoAPI_Fuse.hxx>
#include <BRepAlgoAPI_Cut.hxx>
#include <BRepAlgoAPI_Common.hxx>
#include <BRepBuilderAPI_Transform.hxx>

#include <StlAPI_Writer.hxx>

#include <gp.hxx>
#include <gp_Pnt.hxx>
#include <TopExp_Explorer.hxx>
#include <TopoDS.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Face.hxx>
#include <Poly_Triangulation.hxx>
#include <TColgp_Array1OfDir.hxx>
#include <Poly_Connect.hxx>
#include <StdPrs_ToolShadedShape.hxx>



// Spirit
#include "json_spirit.h"

// Project
#include "WorkerConfig.h"

using namespace std;
using namespace json_spirit;

// TODO: Tesselate in parallel
// TODO: compress 1.00000000 into 1.0 etc. Also reduce precision


map< string, TopoDS_Shape > shapes = map< string, TopoDS_Shape >();

mValue tesselate(string id) {
    TopoDS_Shape shape = shapes[id];
    BRepMesh().Mesh(shape, 0.0125);
    
    mArray indices;
    mArray positions;
    mArray normalArr;
    
    TopExp_Explorer Ex; 
    int index_offset = 0;
    for (Ex.Init(shape,TopAbs_FACE); Ex.More(); Ex.Next()) { 
        
        TopoDS_Face Face = TopoDS::Face(Ex.Current());
        TopLoc_Location Location = TopLoc_Location();
        Handle(Poly_Triangulation) facing = BRep_Tool().Triangulation(Face,Location);
        
        TColgp_Array1OfDir the_normal(facing->Nodes().Lower(), facing->Nodes().Upper());
        Poly_Connect connect(facing);
        StdPrs_ToolShadedShape().Normal(Face, connect, the_normal);
        
        for (int i = 1; i <= facing->NbNodes(); ++i) {
            gp_Pnt vertex = facing->Nodes().Value(i);
            
                
            gp_Pnt transformedVtx = vertex.Transformed(Face.Location().Transformation());
            
            positions.push_back(transformedVtx.X());
            positions.push_back(transformedVtx.Y());
            positions.push_back(transformedVtx.Z());
            
            normalArr.push_back(the_normal(i).X());
            normalArr.push_back(the_normal(i).Y());
            normalArr.push_back(the_normal(i).Z());
        }
        
        for (int i = 1; i <= facing->NbTriangles(); ++i) {
            Poly_Triangle triangle = facing->Triangles().Value(i);
            Standard_Integer index1, index2, index3;
            triangle.Get(index1, index2, index3);
            
            // Step 1 - caluclate the normals of the triangles
            gp_Pnt vertex1 = facing->Nodes().Value(index1);
            gp_Pnt vertex2 = facing->Nodes().Value(index2);
            gp_Pnt vertex3 = facing->Nodes().Value(index3);
            
            indices.push_back(index_offset + index1 - 1);
            indices.push_back(index_offset + index2 - 1);
            indices.push_back(index_offset + index3 - 1);
        }
        
        index_offset += facing->NbNodes();    
    }
    
    mObject result;
    result["primitive"] = "triangles";
    result["positions"] = positions;
    result["normals"] = normalArr;
    result["indices"] = indices;
    
    return result;
}

double get_double(mValue value) {
    if (value.type() == int_type) {
        return (double)value.get_int();
    } else {
        
        return value.get_real();
    }
}

#pragma mark Transforms

TopoDS_Shape translate(map<string, mValue> transform, TopoDS_Shape shape) {
    map< string, mValue > parameters = transform["parameters"].get_obj();
    mValue dx = parameters["dx"];
    mValue dy = parameters["dy"];
    mValue dz = parameters["dz"];
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetTranslation(gp_Vec(get_double(dx), get_double(dy), get_double(dz)));
    
    // Force copy
    BRepBuilderAPI_Transform brep_transform(shape, transformation);
    TopoDS_Shape transformed_shape = brep_transform.Shape();
        
    return transformed_shape;
}

TopoDS_Shape scale(map<string, mValue> transform, TopoDS_Shape shape) {
    map< string, mValue > parameters = transform["parameters"].get_obj();
    mValue x = parameters["x"];
    mValue y = parameters["y"];
    mValue z = parameters["z"];
    mValue factor = parameters["factor"];
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetScale(gp_Pnt(get_double(x), 
                                   get_double(y), 
                                   get_double(z)), 
                            get_double(factor));
    
    // Force copy
    BRepBuilderAPI_Transform brep_transform(shape, transformation);
    TopoDS_Shape transformed_shape = brep_transform.Shape();
    
    return transformed_shape;
}

TopoDS_Shape rotate(map<string, mValue> transform, TopoDS_Shape shape) {
    map< string, mValue > parameters = transform["parameters"].get_obj();
    mValue px = parameters["px"];
    mValue py = parameters["py"];
    mValue pz = parameters["pz"];
    mValue vx = parameters["vx"];
    mValue vy = parameters["vy"];
    mValue vz = parameters["vz"];
    mValue angle = parameters["angle"];
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetRotation(gp_Ax1(gp_Pnt(get_double(px),
                                             get_double(py),
                                             get_double(pz)), 
                                      gp_Dir(get_double(vx),
                                             get_double(vy),
                                             get_double(vz))), 
                               get_double(angle)/180*M_PI);
    
    // Force copy
    BRepBuilderAPI_Transform brep_transform(shape, transformation);
    TopoDS_Shape transformed_shape = brep_transform.Shape();
    
    return transformed_shape;
}


TopoDS_Shape applyTransform(map<string, mValue> transform, TopoDS_Shape shape) {
    mValue transformType = transform["type"];
    if (!transformType.is_null() && (transformType.type() == str_type) && (transformType.get_str() == string("translate"))) {
        return translate(transform, shape);
    }
    if (!transformType.is_null() && (transformType.type() == str_type) && (transformType.get_str() == string("scale"))) {
        return scale(transform, shape);
    }
    if (!transformType.is_null() && (transformType.type() == str_type) && (transformType.get_str() == string("rotate"))) {
        return rotate(transform, shape);
    }
    
    return shape;
}

TopoDS_Shape applyTransforms(TopoDS_Shape shape, map< string, mValue > geometry) {
    TopoDS_Shape transformedShape = shape;
    if (!geometry["transforms"].is_null() && (geometry["transforms"].type() == array_type)) {
        mArray transforms = geometry["transforms"].get_array();
        vector< mValue >::iterator it;
        for (unsigned int k = 0; k < transforms.size(); ++k) {
            mValue transform2 = transforms[k];
            transformedShape = applyTransform(transform2.get_obj(), transformedShape);
        }
    } 
    return transformedShape;
}
    
#pragma mark Primitives

mValue create_cuboid(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue width = parameters["width"];
    mValue depth = parameters["depth"];
    mValue height = parameters["height"];
    if (!width.is_null() && ((width.type() == real_type) || (width.type() == int_type))
        &&
        !depth.is_null() && ((depth.type() == real_type) || (depth.type() == int_type))
        &&
        !height.is_null() && ((height.type() == real_type) || (height.type() == int_type))) {
        
        TopoDS_Shape shape = BRepPrimAPI_MakeBox(get_double(width), 
                                                 get_double(depth), 
                                                 get_double(height)).Shape();
        
        shapes[id] = applyTransforms(shape, geometry);
        return tesselate(id);
    }
    return  mValue("invalid geometry parameters");
}

mValue create_sphere(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue radius = parameters["radius"];
    if (!radius.is_null() && ((radius.type() == real_type) || (radius.type() == int_type))) {
        TopoDS_Shape shape = BRepPrimAPI_MakeSphere(get_double(radius)).Shape();
        shapes[id] = applyTransforms(shape, geometry);
        return tesselate(id);
    }
    return  mValue("invalid geometry parameters");
}

mValue create_cylinder(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue radius = parameters["radius"];
    mValue height = parameters["height"];
    if (!radius.is_null() && ((radius.type() == real_type) || (radius.type() == int_type))
        &&
        !height.is_null() && ((height.type() == real_type) || (height.type() == int_type))) {
        
        TopoDS_Shape shape = BRepPrimAPI_MakeCylinder(get_double(radius), 
                                                      get_double(height)).Shape();
        shapes[id] = applyTransforms(shape, geometry);
        return tesselate(id);
    }
    return  mValue("invalid geometry parameters");
}

mValue create_cone(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue bottom_radius = parameters["bottom_radius"];
    mValue top_radius = parameters["top_radius"];
    mValue height = parameters["height"];
    if (!bottom_radius.is_null() && ((bottom_radius.type() == real_type) || (bottom_radius.type() == int_type))
        &&
        !top_radius.is_null() && ((top_radius.type() == real_type) || (top_radius.type() == int_type))
        &&
        !height.is_null() && ((height.type() == real_type) || (height.type() == int_type))) {
        
        TopoDS_Shape shape = BRepPrimAPI_MakeCone(get_double(bottom_radius), 
                                                  get_double(top_radius), 
                                                  get_double(height)).Shape();
        shapes[id] = applyTransforms(shape, geometry);
        return tesselate(id);
    }
    return  mValue("invalid geometry parameters");
}

mValue create_wedge(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue x1 = parameters["x1"];
    mValue x2 = parameters["x2"];
    mValue y = parameters["y"];
    mValue z = parameters["z"];
    if (!x1.is_null() && ((x1.type() == real_type) || (x1.type() == int_type))
        &&
        !x2.is_null() && ((x2.type() == real_type) || (x2.type() == int_type))
        &&
        !y.is_null() && ((y.type() == real_type) || (y.type() == int_type))
        &&
        !z.is_null() && ((z.type() == real_type) || (z.type() == int_type))) {
        
        TopoDS_Shape shape = BRepPrimAPI_MakeWedge(get_double(x1), 
                                                   get_double(y), 
                                                   get_double(z), 
                                                   get_double(x2)).Shape();
        shapes[id] = applyTransforms(shape, geometry);
        return tesselate(id);
    }
    return  mValue("invalid geometry parameters");
}

mValue create_torus(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue r1 = parameters["r1"];
    mValue r2 = parameters["r2"];
    if (!r1.is_null() && ((r1.type() == real_type) || (r1.type() == int_type))
        &&
        !r2.is_null() && ((r2.type() == real_type) || (r2.type() == int_type))) {
        
        TopoDS_Shape shape = BRepPrimAPI_MakeTorus(get_double(r1), 
                                                   get_double(r2)).Shape();
        shapes[id] = applyTransforms(shape, geometry);
        return tesselate(id);
    }
    return  mValue("invalid geometry parameters");
}

#pragma mark Boolean

mValue create_union(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    if ((parameters["a"].type() == str_type)
        &&
        (parameters["b"].type() == str_type)) {
        string path_a = parameters["a"].get_str();
        string path_b = parameters["b"].get_str();
        
        TopoDS_Shape shape_a = shapes[path_a];
        TopoDS_Shape shape_b = shapes[path_b];
        
        
        TopoDS_Shape boolean_shape = BRepAlgoAPI_Fuse(shape_a, shape_b).Shape();
        shapes[id] = applyTransforms(boolean_shape, geometry);
        return tesselate(id);
    }
    return mValue("invalid union parameters");
}

mValue create_subtract(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    if ((parameters["a"].type() == str_type)
        &&
        (parameters["b"].type() == str_type)) {
        string id_a = parameters["a"].get_str();
        string id_b = parameters["b"].get_str();
        
        TopoDS_Shape shape_a = shapes[id_a];
        TopoDS_Shape shape_b = shapes[id_b];
        
        // It makes more sense to when selecting 'subtract A FROM B'
        TopoDS_Shape boolean_shape = BRepAlgoAPI_Cut(shape_b, shape_a).Shape();
        shapes[id] = applyTransforms(boolean_shape, geometry);
        return tesselate(id);
    }
    return mValue("invalid union parameters");
}

mValue create_intersect(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    if ((parameters["a"].type() == str_type)
        &&
        (parameters["b"].type() == str_type)) {
        string path_a = parameters["a"].get_str();
        string path_b = parameters["b"].get_str();
        
        TopoDS_Shape shape_a = shapes[path_a];
        TopoDS_Shape shape_b = shapes[path_b];
        
        
        TopoDS_Shape boolean_shape = BRepAlgoAPI_Common(shape_a, shape_b).Shape();
        shapes[id] = applyTransforms(boolean_shape, geometry);
        return tesselate(id);
    }
    return mValue("invalid union parameters");
}


mValue create_geometry(string id, map< string, mValue > geometry) {
    mValue geomType = geometry["type"];
    /*
     * Primitives
     */
    TopoDS_Shape shape;
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("cuboid"))) {
        return create_cuboid(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("sphere"))) {
        return create_sphere(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("cylinder"))) {
        return create_cylinder(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("cone"))) {
        return create_cone(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("wedge"))) {
        return create_wedge(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("torus"))) {
        return create_torus(id, geometry);
    } 
    /*
     * Booleans
     */
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("union"))) {
        return create_union(id, geometry);
    }
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("subtract"))) {
        return create_subtract(id, geometry);
    }
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("intersect"))) {
        return create_intersect(id, geometry);
    }
    return mValue("geometry type not found");
}

int read_exact(unsigned char *buf, int len) {
    int i, got=0;
    do {
        if ((i = read(0, buf+got, len-got)) <= 0)
            return(i);
        got += i;
    } while (got<len);
    return len;
}

int read_cmd(unsigned char* buf) {
    int len;
    if (read_exact(buf, 4) != 4)
        return(-1);
    len = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
    return read_exact(buf, len);
}

int write_exact(const char *buf, int len) {
    int i, wrote = 0;
    do {
        if ((i = write(1, buf+wrote, len-wrote)) <= 0)
            return (i);
        wrote += i;
    } while (wrote<len);
    return len;
}

int write_cmd(const char *buf, int len) {
    char size_buf[4];
    size_buf[0] = (len >> 24) & 0xff;
    size_buf[1] = (len >> 16) & 0xff;
    size_buf[2] = (len >> 8) & 0xff;
    size_buf[3] = len & 0xff;
    write_exact(size_buf, 4);
    return write_exact(buf, len);
}

int main (int argc, char *argv[]) {
    
    unsigned char buf[1024*1024];
    int json_size;
    
    while ((json_size = read_cmd(buf)) > 0) {
        
        try {
            mValue value;
            // Set 0-delimiter for conversion to string
            buf[json_size] = 0;
            string json_string = string((char*)buf);
            read(json_string, value);
            
            if (value.type() == obj_type) {
                map< string, mValue > objMap = mObject(value.get_obj());
                
                mValue msgType = objMap["type"];
                mValue id = objMap["id"];
                mValue geometry = objMap["geometry"];
                
                if (!msgType.is_null() && (msgType.type() == str_type) && (msgType.get_str() == string("create"))
                    && 
                    !id.is_null() && (id.type() == str_type)
                    && 
                    !geometry.is_null() && (geometry.type() == obj_type)) {
                    
                    
                    mValue response = create_geometry(id.get_str(), mObject(geometry.get_obj()));
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
                
                mValue filename = objMap["filename"];
                if (!msgType.is_null() && (msgType.type() == str_type) && (msgType.get_str() == string("stl"))
                    && 
                    !id.is_null() && (id.type() == str_type)
                    && 
                    !filename.is_null() && (filename.type() == str_type)) {
                    
                    string filenameStr = filename.get_str();
                    TopoDS_Shape shape = shapes[id.get_str()];
                    
                    StlAPI_Writer writer;
                    writer.Write(shape, filenameStr.c_str());
                    
                    // TODO: Error handling in response
                    mValue response = mValue("ok");
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
                
            }
            
            mValue response = mValue("unknown message");
            string output = write(response);
            write_cmd(output.c_str(), output.size());
            
        } catch (exception& e) {
            mValue response = mValue(e.what());
            string output = write(response);
            write_cmd(output.c_str(), output.size());
        } catch (...) {
            mValue response = mValue("exception!");
            string output = write(response);
            write_cmd(output.c_str(), output.size());
            
        }
        
    }
    
    return 0;
}
