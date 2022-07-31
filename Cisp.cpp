#include <bits/stdc++.h>
using namespace std;

enum cell_type { Symbol, Number, List, Proc, Lambda };

struct environment;

struct cell {
    cell_type type;
    string val;
    vector<cell> vec;
    function<cell(vector<cell>&)> proc;
    environment* env;
    cell(cell_type type = Symbol) : type(type), env(0) {}
    cell(cell_type type, const string& val) : type(type), val(val), env(0) {}
    cell(function<cell(const vector<cell>&)> proc) : type(Proc), env(0) { 
        this->proc = proc; 
    }
};

const cell false_sym(Symbol, "#f");
const cell true_sym(Symbol, "#t");
const cell nil(Symbol, "nil");

struct environment {
    environment(environment* outer = 0) : outer(outer) {}
    environment(const vector<cell>& params, const vector<cell>& args, environment* outer) : outer(outer) {
        for (int i = 0; i < params.size(); i++) {
            env[params[i].val] = args[i];
        }
    }

    map<string, cell>& find(const string& var) {
        if (env.count(var))
            return env;
        if (outer)
            return outer->find(var);
        cout << "unbound symbol '" << var << "'\n";
        exit(1);
    }

    cell& operator[] (const string& var) {
        return env[var];
    }

private:
    map<string, cell> env;
    environment* outer;
};

function<cell(const vector<cell>&)> genBinFun(function<int(int,int)> binOp) {
    return [=](const vector<cell>& c) {
        int ans = stoi(c[0].val);
        for (int i = 1; i < c.size(); i++) ans = binOp(ans, stoi(c[i].val));
        return cell(Number, to_string(ans)); };
}

function<cell(const vector<cell>&)> genCmpFun(function<bool(int, int)> cmpOp) {
    return [=](const vector<cell>& c) {
        int n = stoi(c[0].val);
        for (int i = 1; i < c.size(); i++) if (cmpOp(n, stoi(c[i].val))) return false_sym;
        return true_sym; };
}

cell proc_length(const vector<cell>& c) { return cell(Number, to_string(c[0].vec.size())); }
cell proc_nullp(const vector<cell>& c) { return c[0].vec.empty() ? true_sym : false_sym; }
cell proc_car(const vector<cell>& c) { return c[0].vec[0]; }

cell proc_cdr(const vector<cell>& c) {
    if (c[0].vec.size() < 2) return nil;
    cell result(c[0]);
    result.vec.erase(result.vec.begin());
    return result;
}

cell proc_append(const vector<cell>& c) {
    cell result(List);
    result.vec = c[0].vec;
    for (int i = 0; i < c[1].vec.size(); i++) result.vec.push_back(c[1].vec[i]);
    return result;
}

cell proc_cons(const vector<cell>& c) {
    cell result(List);
    result.vec.push_back(c[0]);
    for (int i = 0; i < c[1].vec.size(); i++) result.vec.push_back(c[1].vec[i]);
    return result;
}

cell proc_list(const vector<cell>& c) {
    cell result(List); result.vec = c;
    return result;
}

void init_globals(environment& env) {
    env["nil"] = nil;
    env["#f"] = false_sym;
    env["#t"] = true_sym;
    env["append"] = cell(&proc_append);
    env["car"] = cell(&proc_car);
    env["cdr"] = cell(&proc_cdr);
    env["cons"] = cell(&proc_cons);
    env["length"] = cell(&proc_length);
    env["list"] = cell(&proc_list);
    env["null?"] = cell(&proc_nullp);
    env["+"] = cell(genBinFun([=](int a, int b) {return a + b; }));
    env["-"] = cell(genBinFun([=](int a, int b) {return a - b; }));
    env["*"] = cell(genBinFun([=](int a, int b) {return a * b; }));
    env["/"] = cell(genBinFun([=](int a, int b) {return a / b; }));
    env["="] = cell(genCmpFun([=](int a, int b) {return a != b; }));
    env[">"] = cell(genCmpFun([=](int a, int b) {return a <= b; }));
    env["<"] = cell(genCmpFun([=](int a, int b) {return a >= b; }));
    env["<="] = cell(genCmpFun([=](int a, int b) {return a > b; }));
    env[">="] = cell(genCmpFun([=](int a, int b) {return a < b; }));
}

cell eval(cell x, environment* env) {
    if (x.type == Symbol)
        return env->find(x.val)[x.val];
    if (x.type == Number)
        return x;
    if (x.vec.empty())
        return nil;
    if (x.vec[0].type == Symbol) {
        if (x.vec[0].val == "quote")
            return x.vec[1];
        if (x.vec[0].val == "if")
            return eval(eval(x.vec[1], env).val == "#f" ? (x.vec.size() < 4 ? nil : x.vec[3]) : x.vec[2], env);
        if (x.vec[0].val == "set!")
            return env->find(x.vec[1].val)[x.vec[1].val] = eval(x.vec[2], env);
        if (x.vec[0].val == "define")
            return (*env)[x.vec[1].val] = eval(x.vec[2], env);
        if (x.vec[0].val == "lambda") {
            x.type = Lambda;
            x.env = env;
            return x;
        }
        if (x.vec[0].val == "begin") {
            for (int i = 1; i < x.vec.size() - 1; ++i)
                eval(x.vec[i], env);
            return eval(x.vec.back(), env);
        }
    }
    cell proc(eval(x.vec[0], env));
    vector<cell> exps;
    for (int i = 1; i < x.vec.size(); i++) {
        exps.push_back(eval(x.vec[i], env));
    }
    if (proc.type == Lambda) {
        return eval(proc.vec[2], new environment(proc.vec[1].vec, exps, proc.env));
    }
    else if (proc.type == Proc)
        return proc.proc(exps);
    cout << "not a function\n";
    exit(1);
}

list<string> tokenize(const string& s) {
    list<string> tokens;
    for (int i = 0; i < s.size();) {
        while (s[i] == ' ' || s[i] == '\t') i++;
        if (s[i] == '(' || s[i] == ')') {
            tokens.push_back(s[i] == '(' ? "(" : ")"); i++;
        }
        else {
            int j = i;
            while (j < s.size() && s[j] != ' ' && s[j] != '(' && s[j] != ')') j++;
            if (j > i) tokens.push_back(s.substr(i, j - i));
            i = j;
        }
    }
    return tokens;
}

cell atom(const string& token) {
    if (isdigit(token[0]) || (token[0] == '-' && isdigit(token[1])))
        return cell(Number, token);
    return cell(Symbol, token);
}

cell read_from(list<string>& tokens) {
    const string token = tokens.front();
    tokens.pop_front();
    if (token == "(") {
        cell c(List);
        while (!tokens.empty() && tokens.front() != ")")
            c.vec.push_back(read_from(tokens));
        if (!tokens.empty()) tokens.pop_front();
        else cout << "the brackets don't match!\n";
        return c;
    }
    else
        return atom(token);
}

cell read(const string& s) {
    list<string> tokens(tokenize(s));
    return read_from(tokens);
}

string toString(const cell& exp) {
    if (exp.type == List) {
        string s("(");
        for (int i = 0; i < exp.vec.size(); i++) s += toString(exp.vec[i]) + ' ';
        if (s.back() == ' ')
            s.pop_back();
        return s + ')';
    }
    else if (exp.type == Lambda)
        return "<Lambda>";
    else if (exp.type == Proc)
        return "<Proc>";
    return exp.val;
}

int main() {
    environment global_env; init_globals(global_env);
    cout << "1.file, 2.repl: ";
    int mode = 1;
    cin >> mode; cin.ignore();
    if (mode == 1) {
        cout << "please input the file path:\n";
        string path = "in.lisp", line, tot; cin >> path;
        fstream f; f.open(path);
        while (getline(f, line)) tot += line + ' ';
        cout << toString(eval(read(tot), &global_env)) << '\n';
    }
    else {
        while (1) {
            cout << "Cisp>";
            string line; getline(cin, line);
            cout << toString(eval(read(line), &global_env)) << '\n';
        }
    }
}