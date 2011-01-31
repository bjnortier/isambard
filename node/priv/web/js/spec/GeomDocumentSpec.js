describe('GeomDocument', function() {
    
    var doc;
    beforeEach(function() {
        doc = new GeomDocument();
    });

    it('should be empty on creation', function() {
        expect(doc.rootNodes.length).toEqual(0);
    });

    it('can accept and reject root nodes', function() {
        
        var node1 = new GeomNode({type: 'sphere'});
        var node2 = new GeomNode({type: 'cuboid'});
        doc.add(node1);
        doc.add(node2);
        expect(doc.rootNodes.length).toEqual(2);
        expect(doc.rootNodes[1]).toEqual(node1);
        expect(doc.rootNodes[0]).toEqual(node2);

        doc.remove(node1);
        expect(doc.rootNodes.length).toEqual(1);
        expect(doc.rootNodes[0]).toEqual(node2);

        doc.remove(node2);
        expect(doc.rootNodes.length).toEqual(0);
        
    });
    
    it('can be used to find nodes', function() {
        var node1 = new GeomNode({type: 'sphere', path: '/1'});
        var node2 = new GeomNode({type: 'cuboid', path: '/2'});
        doc.add(node1);
        doc.add(node2);
        
        expect(doc.findByPath('/1')).toEqual(node1);
        expect(doc.findByPath('/2')).toEqual(node2);
    });

    it('can be used to find child nodes', function() {
        var node1 = new GeomNode({type: 'sphere', path: '/1'});
        var node2 = new GeomNode({type: 'cuboid', path: '/2'}, [node1]);
        doc.add(node2);
        
        expect(doc.findByPath('/1')).toEqual(node1);
        expect(doc.findByPath('/2')).toEqual(node2);
    });

    it('can manipulate transforms to a node', function() {
        var node1 = new GeomNode({type: 'sphere', path: '/1'});
        doc.add(node1);

        var transform = new Transform({type: 'translate',
                                       prototype: true,
                                       parameters: {dx: 1,
                                                    dy: 1,
                                                    dz: 1}});
        doc.addTransformToNodeWithPath('/1', transform);
        expect(node1.transforms.length).toEqual(1);

        expect(function() {
            doc.addTransformToNodeWithPath('/1', transform)
        }).toThrow(new Error('multiple prototype transforms not allowed'));

        // Remove
        doc.removeTransformFromNodeWithPath('/1', transform);
        expect(node1.transforms.length).toEqual(0);
    });
       

});