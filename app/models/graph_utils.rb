class GraphUtils

  def self.assign_graders(submissions, graders, weights, prohibitions)
    # Given a list of submissions, a list of graders,
    #   a mapping of graders to weights of how much they should work,
    #   and a mapping of students who cannot be graded by particular graders,
    #   assign each submission to a grader, if possible.
    # Runs in time O(V^3) = O((submissions + graders) ^ 3) = O(submissions ^ 3)
    #   when graders < submissions, which is basically always.
    # We could implement the highest-level rule, so that it runs in time O(V^2 E^1/2),
    #   but E = submissions * graders, so we'd get O(submissions ^ 2.5) at best,
    #   and the inefficiencies due to lack of decent data structures means we
    #   wouldn't gain much after all the bookkeeping is done.
    total_weight = weights.values.sum
    sub_to_n = {}
    grader_to_n = {}
    n_to_grader = {}
    n = 1
    submissions.each do |s|
      sub_to_n[s.id] = n
      n += 1
    end
    graders.each do |g|
      grader_to_n[g.id] = n
      n_to_grader[n] = g
      n += 1
    end
    c = {}
    c[0] = {}
    # Connect 1 unit of flow from the source to each submission
    submissions.each do |s|
      c[0][sub_to_n[s.id]] = 1
    end
    # Connect an appripriate weight of work from each grader to the sink
    graders.each do |g|
      c[grader_to_n[g.id]] = {}
      c[grader_to_n[g.id]][n] = ((weights[g.id] / total_weight) * submissions.size).ceil
    end
    # Connect submissions to permitted graders by 1 unit of flow
    submissions.includes(:users).each do |s|
      c[sub_to_n[s.id]] = {}
      graders.each do |g|
        if prohibitions[g.id].to_set.disjoint?(s.users.map(:id).to_set)
          c[sub_to_n[s.id]][g.id] = 1
        end
      end
    end
    # Compute flow
    f = GraphUtils.new.relabel_to_front(c, 0, n)
    # Compute results
    ans = {graders: {}, unfinished: []}
    submissions.each do |s|
      gid = f[sub_to_n[s.id]].find{|gid, gflow| gflow > 0}&.first
      if gid
        ans[:graders][n_to_grader[gid]] ||= []
        ans[:graders][n_to_grader[gid]] << s
      else
        ans[:unfinished] << s
      end
    end
    ans
  end

  private
  # based on https://en.wikipedia.org/wiki/Push%E2%80%93relabel_maximum_flow_algorithm
  # runs in O(V^3)
  def relabel_to_front(c, source, sink)
    @c = c
    @n = c.size # c is the capacity matrix
    @f = {} # f is the pre-flow
    # residual capacity from u to v is c[u][v] - f[u][v]
    def residual(u, v)
      @c.dig(u, v).to_i - @f.dig(u, v).to_i
    end

    @height = (0...@n).map{|_| 0} # height of node
    @excess = (0...@n).map{|_| 0} # flow into node minus flow from node    
    @seen   = (0...@n).map{|_| 0} # neighbors seen since last relabel

    # nodelist is the work queue: critical to shuffle this before processing
    # to ensure a random maxflow
    @nodelist = (0...@n).to_set.subtract([source, sink]).to_a.shuffle!

    def min(a, b) # avoiding array allocs just to compare two numbers
      a < b ? a : b
    end
    
    def push(u, v)
      send = min(@excess[u], residual(u, v))
      @f[u] ||= {}
      @f[u][v] = @f[u][v].to_i + send
      @f[v] ||= {}
      @f[v][u] = @f[v][u].to_i - send
      @excess[u] -= send
      @excess[v] += send
    end

    def relabel(u)
      min_height = Float::INFINITY
      (0...@n).each do |v|
        if residual(u, v) > 0
          min_height = min(min_height, @height[v])
          @height[u] = min_height + 1
        end
      end
    end

    def discharge(u)
      while @excess[u] > 0
        if @seen[u] < @n
          v = @seen[u]
          if residual(u, v) > 0 && @height[u] > @height[v]
            push(u, v)
          else
            @seen[u] += 1
          end
        else
          relabel(u)
          @seen[u] = 0
        end
      end
    end

    # Initialize the preflow
    @height[source] = @n
    @excess[source] = Float::INFINITY
    (0...@n).each do |v|
      push(source, v)
    end

    p = 0
    while p < @nodelist.size
      u = @nodelist[p]
      old_height = @height[u]
      discharge(u)
      if @height[u] > old_height
        @nodelist.unshift(@nodelist.delete_at(p))
        p = 0
      else
        p += 1
      end
    end

    return @f
  end
  
end
