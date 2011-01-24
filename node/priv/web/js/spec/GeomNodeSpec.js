describe("GeomNode", function() {
    
    beforeEach(function() {
    });

    it("should be created from args", function() {

        expect(function() {
            new GeomNode({});
        }).toThrow("type is not defined");

        var node = new GeomNode({type: "cuboid", parameters: {width: 1, depth: 1, height: 1}});
        expect(node.type).toEqual("cuboid");

        expect(node.json).toBeDefined();
        expect(JSON.parse(node.json())).toMatch({type: "cuboid"});
    });

    it("can have empty parameters", function() {

        var node = new GeomNode({type: "union"});
        expect(node.type).toEqual("union");
        expect(JSON.parse(node.json())).toMatch({type: "union"});
    });

    it("can have children", function() {
        
        var child1 = new GeomNode({type: "sphere"});
        var child2 = new GeomNode({type: "cuboid"});
        var parentNode = new GeomNode({type: "union"}, child1, child2);

        expect(parentNode.children.length).toEqual(2);
        expect(parentNode.children[0]).toEqual(child1);
        expect(parentNode.children[1]).toEqual(child2);

        expect(child1.parent).toEqual(parentNode);
        expect(child2.parent).toEqual(parentNode);
    });



});