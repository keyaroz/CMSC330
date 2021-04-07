class Graph
    attr_reader :edges

    def initialize
        clear
    end

    def clear
        @edges = {}
        nil
    end

    # Adds vertex v to the graph if the vertex does not exist in the graph.
    def addVertex(v)
        raise ArgumentError, "vertex '#{v}' already exists" if @edges.has_key? v
        @edges[v] = []
        nil
    end

    # Determines if a vertex is present in the graph
    def hasVertex?(v)
        @edges.has_key? v
    end

    # Returns an array containing all vertices in the graph
    def vertices
        @edges.keys
    end

    # Checks if the graph is empty
    def hasVertices?
        !@edges.empty?
    end

    # Adds an edge originating at from and terminating at to if both vertices are present 
    # within the graph, from and to are not the same, and the edge does not already exist
    # within the graph.
    def addEdge(from, to)
        raise ArgumentError, "from vertex '#{from}' isn't in the graph" unless @edges.has_key? from
        raise ArgumentError, "to vertex '#{to}' isn't in the graph" unless @edges.has_key? to
        raise ArgumentError, "from and to edges are equal" if from == to
        raise ArgumentError, "edge from #{from} to #{to} already exists" if @edges[from].include? to
        @edges[from].push to
        nil
    end

    # Checks if an edge is present within a graph
    def hasEdge?(from, to)
        (hasVertex? from) && (@edges[from].include? to)
    end

    # Performs a breadth first search of the graph starting from origin
    # and returns a hash whose keys are the vertices of the graph, and
    # whose values are the the distance from origin to the vertex. The
    # distance from origin to itself is 0.
    def bfs(origin)
        raise ArgumentError unless @edges.has_key? origin

        q = [[origin, 0]]
        r = {}
        v = [origin]

        while not q.empty?
            curr, dist = q.shift
            r[curr] = dist
            unvisited_neighbors = @edges[curr].select{|e| not v.include? e}
            unvisited_neighbors.each{|n|
                v.push n
                q.push [n, dist + 1]
            }
        end

        r
    end
end


#-------------------------------------------------------------------------
class Graph
    def initialize # do not change, add to, or delete this method
        @g = { }
    end

    #4.1
    def addEdge(str)
        if str =~ /^start: ([A-Za-z]+\-\d+) end: ([A-Za-z]+\-\d+)$/
            if !@g.has_key? $1
                @g[$1] = $2 #maps start to end
            end
        end
    end

    #4.2
    def inDegree(node)
        count = 0
        for start in @g.keys
            if @g[start].include? node
                count = count + 1
            end
        end
        count
    end

    # Another way of doing 4.2
    def inDegree(node)
        count = 0
        
        @g.each { |k,v|
            if @g[k].include? node 
                count = count + 1
            end
        }

        return count

    end





    def outDegree(node)
       count = 0

       if @g[node] != nil
        return @g.[node].length
       end

end










let partial_sum flt lst =
fold (fun acc el -> if el >= flt then el+acc else acc) 0.0 lst

;;





















