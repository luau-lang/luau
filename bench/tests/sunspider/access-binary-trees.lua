--[[
   The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
]]

local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

function TreeNode(left,right,item)
   local this = {}
   this.left = left;
   this.right = right;
   this.item = item;

   this.itemCheck = function(self)
      if (self.left==nil) then return self.item;
      else return self.item + self.left:itemCheck() - self.right:itemCheck(); end
   end

   return this
end

function bottomUpTree(item,depth)
   if (depth>0) then
      return TreeNode(
          bottomUpTree(2*item-1, depth-1)
         ,bottomUpTree(2*item, depth-1)
         ,item
      );
   else
      return TreeNode(nil,nil,item);
   end
end

local ret = 0;

for n = 4,7,1 do
    local minDepth = 4;
    local maxDepth = math.max(minDepth + 2, n);
    local stretchDepth = maxDepth + 1;

    local check = bottomUpTree(0,stretchDepth):itemCheck();

    local longLivedTree = bottomUpTree(0,maxDepth);

    for depth = minDepth,maxDepth,2 do
        local iterations = 2.0 ^ (maxDepth - depth + minDepth - 1) -- 1 << (maxDepth - depth + minDepth);

        check = 0;
        for i = 1,iterations do
            check = check + bottomUpTree(i,depth):itemCheck();
            check = check + bottomUpTree(-i,depth):itemCheck();
        end
    end

    ret = ret + longLivedTree:itemCheck();
end

local expected = -4;

if (ret ~= expected) then
    assert(false, "ERROR: bad result: expected " .. expected .. " but got " .. ret);
end

end

bench.runCode(test, "access-binary-trees")
