class Numeric
  def clamp(lo, hi)
    if self != self
      lo
    else 
      [lo, [self, hi].min].max
    end
  end
end
