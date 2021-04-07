require_relative "graph.rb"

class Synsets
    def initialize
        @Hash = {}
    end

    def load(synsets_file)
    
        invalid_bool = false
        tempHash = {}
        invalid_Arr = []
        invalid_line = 1

        File.readlines(synsets_file).each do |line|                
            if line =~ (/id: (\d+) synset: (\S+)/)      

                if ((@Hash.has_key? $1.to_i) || (tempHash.has_key? $1.to_i))  # if the temp hash OR the original has this ID already
                    invalid_bool = true
                    invalid_Arr.push(invalid_line)                

                else 
                    tempHash[$1.to_i] = $2.split(",")           
                end
          
            else     # if line DOES NOT MATCH (/^id: (\d+) synset: (\S+)$/)
                invalid_bool = true
                invalid_Arr.push(invalid_line)
            end

            invalid_line = invalid_line + 1
        end # end of read lines iteration
        
        if invalid_bool == true   # now checking if any line was invalid
            return invalid_Arr 
                          
        else                      # if all lines were valid, return hash
            for k,v in tempHash
                @Hash[k] = v
            end
            return nil          
        end                                                    

    end

    def addSet(synset_id, nouns)
        # If the synset ID is negative, or the list of nouns is empty, or the synset 
        # ID has already been defined, return false without modifying the current object. 
        # Otherwise return true and store the new synset.
        if ((synset_id < 0) || (nouns.length == 0))
            return false
        end

        @Hash[synset_id] = nouns
        true

    end

    def lookup(synset_id)                # WHY IS THIS FAILING ???????
       # Given a synset return an array of the nouns 
       # in the synset. If there is no entry for the
       # requested ID, return an empty array.
       
        array = Array.new
        if @Hash.has_key? synset_id
            return @Hash[synset_id]
        end
           return array
      
    end

    def findSynsets(to_find) # Takes array, hash, or nil
        # If a String is provided, this method should return
        # an array of 0 or more synset IDs corresponding 
        # to synsets containing this noun.
        tempHash = {}
        arr = Array.new
        outputArr = []
        if to_find.is_a? String    # if its a string, look through hash to see which ID(key) maps to an array that has to_find
            for k,v in @Hash
                if ( (v.include? to_find) || (@Hash[k] == to_find))    # if ID maps to an array that includes to_find OR maps to to_find
                    
                    outputArr.push(k.to_i)    # put the synsetID in an array
                end
            end

            return outputArr # return array of synset IDs
        end # end of if string

        nounToIDs = Hash.new # a hash to map noun to IDS
        arr = []

        # IF IT'S AN ARRAY
        for noun in to_find                # going through the given array of nouns
            for k,v in @Hash
                if (v.include? noun) || (@Hash[k] == noun)       # v is an array of nouns, i is each noun in to_find
                    arr.push(k.to_i)    # save noun's ID in an array
                    
                end  
                
            end
            nounToIDs[noun] = arr # then map the noun to an array of IDs that share that noun in common
            arr = Array.new
        end
        nounToIDs
    end
end

class Hypernyms # construct and operate on a graph
    def initialize
        @graph = Graph.new
    end

    def load(hypernyms_file)
        invalid_bool = false
        tempHash = {}
        invalid_Arr = []
        invalid_line = 1

        File.readlines(hypernyms_file).each do |line|     
    
            if line =~ (/from: (\d+) to: ([,\d+]+)/)  # if the line is valid    
                    from = $1.to_i
                    tooo = $2.split(",")

                    to = tooo.collect {|ele| ele.to_i}
                    tempHash[from] = to           
            
            else                                  # if it doesnt match the regex, store the line number
                invalid_bool = true
                invalid_Arr.push(invalid_line)      # stores invalid line numbers
            end

            invalid_line = invalid_line + 1 #increment line number 

        end # end of read lines iteration
 
        if invalid_bool == true   #For some reason invalid bool is true whenit shouldnt be  # looking for the Synset ID --> $1
            return invalid_Arr 
                    
        else                                      
            tempHash.each do |k,v|               
                v.each do |x| 
                    addHypernym(k.to_i, x.to_i)
                end
            end
            return nil          
        end                                 
    end

    def addHypernym(source, destination) #Add hypernym to graph
        if (source < 0) || (destination < 0) || (source == destination)
            return false
        end

        if !(@graph.hasVertex?(source))
            @graph.addVertex(source)
        end
        if !(@graph.hasVertex?(destination))
            @graph.addVertex(destination)
        end
        if !(@graph.hasEdge?(source, destination))
            @graph.addEdge(source, destination)
        end
        return true
    end

    def lca(id1, id2)
        output = []
        vertexToDistance = {}
        id1 = id1.to_i
        id2 = id2.to_i

        if (@graph.hasVertex? id1) && (@graph.hasVertex? id2)
            
            second = @graph.bfs(id2)

            (@graph.bfs(id1)).each do |id, distance|
                if second.include? id
                    vertexToDistance[id] = distance + second[id]
                end
            end
        
            min_distance = vertexToDistance.values.min
            vertexToDistance.each do |k, v|
                if v == min_distance
                    output.push(k)
                end

            end
            return output
        else # if !graph.hasVertex
            return nil  
        end
    end
end

class CommandParser
    def initialize
        @synsets = Synsets.new
        @hypernyms = Hypernyms.new
    end

    def parse(command)
        line_arr = command.split(" ")
        outputHash = {}

        if line_arr[0] == "load"
            outputHash[:recognized_command] = :load
            if line_arr.length > 3 
                outputHash[:result] = :error
            else
                if (@synsets.load(line_arr[1]) == nil) && (@hypernyms.load(line_arr[2]) == nil)
                    outputHash[:result] = true 
                    puts @hypernyms.inspect
                else
                    outputHash[:result] = false 
                    puts @hypernyms.inspect
                end
            end
        

        elsif line_arr[0] == "lookup"
            outputHash[:recognized_command] = :lookup
            if line_arr.length > 2 || !(line_arr[1] =~ /^(\d+)$/) #if more than 2 arguements or doesn't match regex            
                outputHash[:result] = :error
            else # if it's valid
                outputHash[:result] = @synsets.lookup($1.to_i)
            end           
        

        elsif line_arr[0] == "find"
            outputHash[:recognized_command] = :find
            if line_arr[1] =~ /^(\w+)$/             
                outputHash[:result] = @synsets.findSynsets($1)
            else
                outputHash[:result] = :error
            end 
        

        elsif line_arr[0] == "findmany"
            outputHash[:recognized_command] = findmany
            if line_arr[1] =~ /^(\w+[,\w]*)$/
                arr = line_arr[1].split(",")
                @synsets.findSynsets(arr)
            else
                outputHash[:result] = :error
            end
            
        

        elsif line_arr[0] == "lca"
            outputHash[:recognized_command] = :lca
            if line_arr[1] =~ /^\s*(\d+)\s*(\d+)*/
                @hypernyms.lca($1.to_i, $2.to_i)
            else
                outputHash[:result] = :error
            end
        
        else
            outputHash[:recognized_command] = :invalid
        end

        return outputHash

    end
end







pos = 1

if (i=n; i>=2; i--) {


    # rem is the last block of s values we will read/write
    if (n%s == 0){
        rem = (n/s)
    }
    else{       # id n / s has a remainder
        rem = (n/s)+1
    }

    for(j = 1; i < rem; i++){ # i down to 2
        bool = false
        if( pos + s > n){
            read(pos-1, pos-1, n-pos)
            bool = true
        }
        else {
            read(pos-1, pos-1, s)
            pos += s
        }

        if(flag == false)
            for(j= pos; j < (pos + s - 1)){
                if R[j] > R[j+1]
                    R[j] swap with R[j+1]
                }
            }
            write(pos, pos, s)
        }
    }
}





