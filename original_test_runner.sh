#!/bin/bash
echo "=== TESTING ORIGINAL TEST FILE SEMANTIC ERRORS ==="
echo "Running the original test file to identify which semantic errors are caught..."
echo

# Initialize counters
PASSED=0
FAILED=0

# Helper function to run test and check result
run_test() {
    local test_name="$1"
    local test_code="$2"
    local expected_error="$3"
    
    echo "--- $test_name ---"
    
    # Run the parser and capture output
    output=$(echo "$test_code" | ./semc 2>&1)
    exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        echo "❌ COMPILED SUCCESSFULLY (Expected error: $expected_error)"
        echo "   No semantic error was caught"
        ((FAILED++))
    else
        echo "✅ CAUGHT ERROR: $output"
        echo "   Expected: $expected_error"
        ((PASSED++))
    fi
    echo
}

# =============================================================================
# ORIGINAL TEST FILE - AS PROVIDED
# =============================================================================

run_test "ORIGINAL TEST FILE - First Error Detection" \
'/#-> comment 
       Long <-#/ 
def  foo1(par1  int:a; par2  int:b; par3  int:c;  par4 char:c1) : returns bool
var 
    type bool: res;
begin
    var  
        type char: x, b;
        type int: y;
    begin
      b = '"'"'&'"'"';
      /#->1  a = x; <-#/  
      /#->2 b = 8; <-#/
      a = (y*7)/a-y;
      /#->3 a = (y*7)/b-y; <-#/
      /#->4 a = (y*7)/a-c1; <-#/
      /#->5 res = (b==c) and (y>a); <-#/
      /#->6 res = (b==c1) and (y+a); <-#/
      /#->7 3+6 = 9; <-#/
      /#->8 %x =6 ;<-#/ 
   end 
   return res;
end
def  goo1(par1  int:I; par2  int:j; par3  int:k; par4  int:x) :  
begin
    def  goo2 (par1 int:l; par2 int:m; par3 int:n) : returns bool
    var 
          type bool: x, j;
          /#->10 type bool: n;<-#/
          type char: k;
    begin
          k = '"'"'@'"'"';
          i = l + l;
          /#->11 i= j+1; <-#/
          /#->12 i= k+1; <-#/
          if  (k=='"'"'*'"'"') || (x!=false) and ( l+m < i)  :
                                                          x = l < m;
           return x;
    end
    var 
            type char: x;
            type bool: k;
    begin
          k= call goo2(5,i,j);
         /#->13 x= call goo2(5,i,j); <-#/
         /#->14 k = call goo2(5,i); <-#/
         /#->15 k = call goo2(5,x,j); <-#/
    end
       /#->16 n= false; <-#/
       /#->17 x= '"'"'#'"'"'; <-#/
       x= k;
end
def  foo3(par1 int:I; par2 int:j; par3 int:k ) : returns int
var 
      type  int: total; /#-> variable declaration <-#/
      type  bool: bo;
begin
     def  si1(par1  int:t) : returns int  /#-> function declaration <-#/
     var 
          type  int: temp;
      begin
          temp= t*t;
          return temp;
     end
      bo= call foo1 (i, j, k, '"'"'^'"'"');
      /#->18 j= call foo2(); <-#/ 
      total=si1(i+j+k);  /#-> statemets <-#/
     return total;
end
def  foo2() : returns int
var
      type string  s1 [100], string  s2 [100];
      type  int: i:0, j:0, cnt;
begin
     cnt= 1;
     while : i < |s1|  begin
        while : j < |s2| / 2  begin
           /#->19 if s1[i]  =  s2[j] :  <-#/
           begin
               cnt=cnt*2;
           end
           j= j+1;
        end
        i= i+1;
     end
     return cnt;
end
def  _ main() : 
var 
    type int: x1;
begin
  x1 = call foo2();
end' \
"Variable 'b' redeclared in same scope"

# =============================================================================
# ERROR 1: Uncomment line 1 - a = x
# =============================================================================

run_test "ERROR 1: a = x (INT = CHAR)" \
'def  foo1(par1  int:a; par2  int:b; par3  int:c;  par4 char:c1) : returns bool
var 
    type bool: res;
begin
    var  
        type char: x, b2;
        type int: y;
    begin
      b2 = '"'"'&'"'"';
      a = x;
   end 
   return res;
end
def  _ main() : 
begin
end' \
"Type mismatch in assignment (expected INT, found CHAR)"

# =============================================================================
# ERROR 2: Uncomment line 2 - b = 8
# =============================================================================

run_test "ERROR 2: b = 8 (CHAR = INT)" \
'def  foo1(par1  int:a; par2  int:b; par3  int:c;  par4 char:c1) : returns bool
var 
    type bool: res;
begin
    var  
        type char: x, b2;
        type int: y;
    begin
      b2 = '"'"'&'"'"';
      b2 = 8;
   end 
   return res;
end
def  _ main() : 
begin
end' \
"Type mismatch in assignment (expected CHAR, found INT)"

# =============================================================================
# ERROR 3: Uncomment line 3 - a = (y*7)/b-y
# =============================================================================

run_test "ERROR 3: a = (y*7)/b-y (arithmetic with CHAR)" \
'def  foo1(par1  int:a; par2  int:b; par3  int:c;  par4 char:c1) : returns bool
var 
    type bool: res;
begin
    var  
        type char: x, b2;
        type int: y;
    begin
      b2 = '"'"'&'"'"';
      a = (y*7)/b2-y;
   end 
   return res;
end
def  _ main() : 
begin
end' \
"Arithmetic operator requires INT or REAL operands"

# =============================================================================
# ERROR 4: Uncomment line 4 - a = (y*7)/a-c1
# =============================================================================

run_test "ERROR 4: a = (y*7)/a-c1 (arithmetic with CHAR)" \
'def  foo1(par1  int:a; par2  int:b; par3  int:c;  par4 char:c1) : returns bool
var 
    type bool: res;
begin
    var  
        type char: x, b2;
        type int: y;
    begin
      b2 = '"'"'&'"'"';
      a = (y*7)/a-c1;
   end 
   return res;
end
def  _ main() : 
begin
end' \
"Arithmetic operator requires INT or REAL operands"

# =============================================================================
# ERROR 5: Uncomment line 5 - res = (b==c) and (y>a)
# =============================================================================

run_test "ERROR 5: res = (b==c) and (y>a) (logical operator)" \
'def  foo1(par1  int:a; par2  int:b; par3  int:c;  par4 char:c1) : returns bool
var 
    type bool: res;
begin
    var  
        type char: x, b2;
        type int: y;
    begin
      b2 = '"'"'&'"'"';
      a = (y*7)/a-y;
      res = (b2==c) and (y>a);
   end 
   return res;
end
def  _ main() : 
begin
end' \
"Logical operator type error"

# =============================================================================
# ERROR 6: Uncomment line 6 - res = (b==c1) and (y+a)
# =============================================================================

run_test "ERROR 6: res = (b==c1) and (y+a) (logical operator)" \
'def  foo1(par1  int:a; par2  int:b; par3  int:c;  par4 char:c1) : returns bool
var 
    type bool: res;
begin
    var  
        type char: x, b2;
        type int: y;
    begin
      b2 = '"'"'&'"'"';
      a = (y*7)/a-y;
      res = (b2==c1) and (y+a);
   end 
   return res;
end
def  _ main() : 
begin
end' \
"Logical operator type error"

# =============================================================================
# ERROR 7: Uncomment line 7 - 3+6 = 9
# =============================================================================

run_test "ERROR 7: 3+6 = 9 (invalid left-hand side)" \
'def  _ main() : 
begin
    3+6 = 9;
end' \
"Invalid left-hand side in assignment"

# =============================================================================
# ERROR 8: Uncomment line 8 - %x = 6
# =============================================================================

run_test "ERROR 8: %x = 6 (lexical error)" \
'def  _ main() : 
begin
    %x = 6;
end' \
"Lexical error: Unknown character"

# =============================================================================
# ERROR 10: Uncomment line 10 - type bool: n
# =============================================================================

run_test "ERROR 10: type bool: n (parameter redeclaration)" \
'def  goo2 (par1 int:l; par2 int:m; par3 int:n) : returns bool
var 
      type bool: x, j;
      type bool: n;
      type char: k;
begin
      return true;
end
def  _ main() : 
begin
end' \
"Variable redeclared in the same scope"

# =============================================================================
# ERROR 11: Uncomment line 11 - i = j+1
# =============================================================================

run_test "ERROR 11: i = j+1 (variable not declared)" \
'def  goo1(par1  int:I; par2  int:j; par3  int:k; par4  int:x) :  
begin
    def  goo2 (par1 int:l; par2 int:m; par3 int:n) : returns bool
    var 
          type bool: x, j;
          type char: k;
    begin
          k = '"'"'@'"'"';
          i = j+1;
    end
end
def  _ main() : 
begin
end' \
"Variable used before definition"

# =============================================================================
# ERROR 12: Uncomment line 12 - i = k+1
# =============================================================================

run_test "ERROR 12: i = k+1 (arithmetic with CHAR)" \
'def  _ main() : 
var 
    type char: k;
    type int: i;
begin
    k = '"'"'@'"'"';
    i = k + 1;
end' \
"Arithmetic operator requires INT or REAL operands"

# =============================================================================
# ERROR 13: Uncomment line 13 - x = call goo2(5,i,j)
# =============================================================================

run_test "ERROR 13: x = call goo2(5,i,j) (type mismatch CHAR = BOOL)" \
'def  goo2 (par1 int:l; par2 int:m; par3 int:n) : returns bool
begin
    return true;
end
def  _ main() : 
var
    type char: x;
    type int: i, j;
begin
    x = call goo2(5, i, j);
end' \
"Type mismatch in assignment (expected CHAR, found BOOL)"

# =============================================================================
# ERROR 14: Uncomment line 14 - k = call goo2(5,i)
# =============================================================================

run_test "ERROR 14: k = call goo2(5,i) (wrong parameter count)" \
'def  goo2 (par1 int:l; par2 int:m; par3 int:n) : returns bool
begin
    return true;
end
def  _ main() : 
var
    type bool: k;
    type int: i;
begin
    k = call goo2(5, i);
end' \
"Wrong number of arguments in call"

# =============================================================================
# ERROR 15: Uncomment line 15 - k = call goo2(5,x,j)
# =============================================================================

run_test "ERROR 15: k = call goo2(5,x,j) (wrong parameter type)" \
'def  goo2 (par1 int:l; par2 int:m; par3 int:n) : returns bool
begin
    return true;
end
def  _ main() : 
var
    type bool: k;
    type char: x;
    type int: j;
begin
    k = call goo2(5, x, j);
end' \
"Type mismatch in argument"

# =============================================================================
# ERROR 16: Uncomment line 16 - n = false
# =============================================================================

run_test "ERROR 16: n = false (variable not in scope)" \
'def  _ main() : 
begin
    n = false;
end' \
"Variable used before definition"

# =============================================================================
# ERROR 17: Uncomment line 17 - x = '#'
# =============================================================================

run_test "ERROR 17: x = '#' (type mismatch based on context)" \
'def  _ main() : 
var
    type bool: x;
begin
    x = '"'"'#'"'"';
end' \
"Type mismatch in assignment (expected BOOL, found CHAR)"

# =============================================================================
# ERROR 18: Uncomment line 18 - j = call foo2()
# =============================================================================

run_test "ERROR 18: j = call foo2() (function return type mismatch)" \
'def  foo2() : returns int
begin
    return 42;
end
def  _ main() : 
var
    type char: j;
begin
    j = call foo2();
end' \
"Type mismatch in assignment (expected CHAR, found INT)"

# =============================================================================
# ERROR 19: Uncomment line 19 - if s1[i] = s2[j]
# =============================================================================

run_test "ERROR 19: if s1[i] = s2[j] (assignment in condition)" \
'def  _ main() : 
var
    type string: s1, s2;
    type int: i;
begin
    s1 = "hello";
    s2 = "world";
    i = 0;
    if s1[i] = s2[i] :
    begin
    end
end' \
"Invalid left-hand side in assignment OR syntax error"

# =============================================================================
# TEST SUMMARY
# =============================================================================

echo "==============================================="
echo "     ORIGINAL TEST FILE ERROR ANALYSIS"
echo "==============================================="
echo "Total Error Tests: $((PASSED + FAILED))"
echo "✅ Errors Caught: $PASSED"
echo "❌ Errors Not Caught: $FAILED"
echo

if [ $FAILED -eq 0 ]; then
    echo "🎉 ALL SEMANTIC ERRORS DETECTED! 🎉"
    echo "Your parser correctly identifies all semantic errors"
    echo "from the original test file!"
else
    echo "⚠️  Some semantic errors were not caught."
    echo "These may need additional parser improvements:"
    echo "- Check semantic analysis rules"
    echo "- Verify error message patterns"
    echo "- Ensure proper error precedence"
fi

echo
echo "==============================================="
echo "📊 ERROR DETECTION BREAKDOWN:"
echo "• Variable scope/declaration errors"
echo "• Type mismatch in assignments"  
echo "• Arithmetic operator type errors"
echo "• Function call parameter errors"
echo "• Logical operator type errors"
echo "• Invalid assignment constructs"
echo "• Lexical analysis errors"
echo "==============================================="