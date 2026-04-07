#include <bits/stdc++.h>
using namespace std;

struct Term{ long long a; int b,c,d; };

struct Poly{
    vector<Term> t;
    void normalize(){
        sort(t.begin(), t.end(), [](const Term&x,const Term&y){
            if (x.b!=y.b) return x.b>y.b; if (x.c!=y.c) return x.c>y.c; if (x.d!=y.d) return x.d>y.d; return x.a>y.a;
        });
        vector<Term> r; r.reserve(t.size());
        for (auto &e: t){
            if (!r.empty() && r.back().b==e.b && r.back().c==e.c && r.back().d==e.d){
                r.back().a += e.a; if (r.back().a==0) r.pop_back();
            } else if (e.a!=0) r.push_back(e);
        }
        t.swap(r);
    }
    static Poly from_const(long long v){ Poly p; if (v) p.t.push_back({v,0,0,0}); return p; }
    static Poly from_term(long long a,int b,int c,int d){ Poly p; if (a) p.t.push_back({a,b,c,d}); return p; }
    Poly operator+(const Poly&o)const{ Poly r; r.t=t; r.t.insert(r.t.end(), o.t.begin(), o.t.end()); r.normalize(); return r; }
    Poly operator-(const Poly&o)const{ Poly r; r.t=t; for(auto e:o.t) r.t.push_back({-e.a,e.b,e.c,e.d}); r.normalize(); return r; }
    Poly operator*(const Poly&o)const{ Poly r; r.t.reserve(t.size()*o.t.size()); for(auto &x:t) for(auto &y:o.t) r.t.push_back({x.a*y.a,x.b+y.b,x.c+y.c,x.d+y.d}); r.normalize(); return r; }
    Poly derivate() const{ Poly r; for(auto &x:t){ if(x.b>0) r.t.push_back({x.a*x.b,x.b-1,x.c,x.d}); if(x.c>0) r.t.push_back({x.a*x.c,x.b,x.c-1,x.d+1}); if(x.d>0) r.t.push_back({-x.a*x.d,x.b,x.c+1,x.d-1}); } r.normalize(); return r; }
    bool is_one() const{ return t.size()==1 && t[0].a==1 && t[0].b==0 && t[0].c==0 && t[0].d==0; }
    static void app(string &s, char c){ s.push_back(c); }
    static void app3(string &s, char a, char b, char c){ s.push_back(a); s.push_back(b); s.push_back(c); }
    string str() const{
        if (t.empty()){ string s; s.push_back('0'); return s; }
        string s; s.reserve(t.size()*8);
        for(size_t i=0;i<t.size();++i){ const Term&e=t[i]; long long coef=e.a; bool first=i==0; if(coef<0){ s.push_back('-'); coef=-coef; } else if(!first) s.push_back('+'); bool has=(e.b||e.c||e.d); if(!has){ s+=to_string(coef); }
            else { if (coef!=1) s+=to_string(coef); if(e.b){ s.push_back('x'); if(e.b!=1){ s.push_back('^'); s+=to_string(e.b);} } if(e.c){ app3(s,'s','i','n'); if(e.c!=1){ s.push_back('^'); s+=to_string(e.c);} s.push_back('x'); } if(e.d){ app3(s,'c','o','s'); if(e.d!=1){ s.push_back('^'); s+=to_string(e.d);} s.push_back('x'); } }
        }
        return s;
    }
};

struct Frac{ Poly p,q; static Frac from_int(long long v){ Frac f; f.p=Poly::from_const(v); f.q=Poly::from_const(1); return f; } static Frac from_poly(const Poly&num){ Frac f; f.p=num; f.q=Poly::from_const(1); return f; }
    Frac operator+(const Frac&o)const{ Frac r; r.p=p*o.q + o.p*q; r.q=q*o.q; return r; }
    Frac operator-(const Frac&o)const{ Frac r; r.p=p*o.q - o.p*q; r.q=q*o.q; return r; }
    Frac operator*(const Frac&o)const{ Frac r; r.p=p*o.p; r.q=q*o.q; return r; }
    Frac operator/(const Frac&o)const{ Frac r; r.p=p*o.q; r.q=q*o.p; return r; }
    Frac derivate() const{ Frac r; r.p=p.derivate()*q - q.derivate()*p; r.q=q*q; return r; }
    string str() const{ if(q.is_one()) return p.str(); string s; s.push_back('('); s+=p.str(); s.push_back(')'); s.push_back('/'); s.push_back('('); s+=q.str(); s.push_back(')'); return s; }
};

struct Parser{
    string s; int n,i; explicit Parser(string ss):s(move(ss)),n((int)s.size()),i(0){}
    static bool isop(char c){ return c=='+'||c=='-'||c=='*'||c=='/'||c=='('||c==')'; }
    void skip(){ while(i<n && isspace((unsigned char)s[i])) ++i; }
    Frac parse(){ skip(); Frac v=expr(); skip(); return v; }
    Frac expr(){ Frac v=term(); while(true){ skip(); if(i<n && (s[i]=='+'||s[i]=='-')){ char op=s[i++]; Frac t=term(); v = (op=='+')? (v+t) : (v-t); } else break; } return v; }
    Frac term(){ Frac v=factor(); while(true){ skip(); if(i<n && (s[i]=='*'||s[i]=='/')){ char op=s[i++]; Frac t=factor(); v = (op=='*')? (v*t) : (v/t); } else break; } return v; }
    Frac factor(){ skip(); int sign=1; while(i<n && (s[i]=='+'||s[i]=='-')){ if(s[i]=='-') sign=-sign; ++i; skip(); }
        if(i<n && s[i]=='('){ ++i; Frac v=expr(); skip(); if(i<n && s[i]==')') ++i; if(sign==-1) v=Frac::from_int(-1)*v; return v; }
        string atom; while(i<n && !isop(s[i])){ atom.push_back(s[i]); ++i; } Frac v=parse_atom(atom); if(sign==-1) v=Frac::from_int(-1)*v; return v; }
    static bool starts_sin(const string&t,int pos){ return pos+2<(int)t.size() && t[pos]=='s' && t[pos+1]=='i' && t[pos+2]=='n'; }
    static bool starts_cos(const string&t,int pos){ return pos+2<(int)t.size() && t[pos]=='c' && t[pos+1]=='o' && t[pos+2]=='s'; }
    static Frac parse_atom(const string&atom){ int pos=0,m=(int)atom.size(); long long sgn=1; if(pos<m && (atom[pos]=='+'||atom[pos]=='-')){ if(atom[pos]=='-') sgn=-1; ++pos; }
        long long coef=0; bool has=false; while(pos<m && isdigit((unsigned char)atom[pos])){ has=true; coef=coef*10+(atom[pos]-'0'); ++pos; } if(!has) coef=1; coef*=sgn;
        int bx=0, sc=0, cc=0; while(pos<m){ if (atom[pos]=='x'){ ++pos; int e=1; if(pos<m && atom[pos]=='^'){ ++pos; long long v=0; bool dig=false; while(pos<m && isdigit((unsigned char)atom[pos])){ dig=true; v=v*10+(atom[pos]-'0'); ++pos; } e = dig ? (int)v : 0; } bx += e; }
            else if (starts_sin(atom,pos)){ pos+=3; int e=1; if(pos<m && atom[pos]=='^'){ ++pos; long long v=0; bool dig=false; while(pos<m && isdigit((unsigned char)atom[pos])){ dig=true; v=v*10+(atom[pos]-'0'); ++pos; } e = dig ? (int)v : 0; } if(pos<m && atom[pos]=='x') ++pos; sc += e; }
            else if (starts_cos(atom,pos)){ pos+=3; int e=1; if(pos<m && atom[pos]=='^'){ ++pos; long long v=0; bool dig=false; while(pos<m && isdigit((unsigned char)atom[pos])){ dig=true; v=v*10+(atom[pos]-'0'); ++pos; } e = dig ? (int)v : 0; } if(pos<m && atom[pos]=='x') ++pos; cc += e; }
            else { ++pos; }
        }
        return Frac::from_poly(Poly::from_term(coef,bx,sc,cc)); }
};

int main(){ ios::sync_with_stdio(false); cin.tie(nullptr); string expr; if(!getline(cin,expr)) return 0; Parser p(expr); Frac f=p.parse(); Frac df=f.derivate(); cout<<f.str()<<'\n'<<df.str()<<'\n'; return 0; }
