#include <vector>
#include <string>
#include <algorithm>

struct RefSeqInfo {
    RefSeqInfo(std::string s, double v)
        :str(s)
        ,val(v) { }
    bool operator <(const RefSeqInfo& rhs) const { return str < rhs.str; }
    std::string str;
    double val;
};

struct RefSeqInfoVector {
    RefSeqInfoVector() { }
    ~RefSeqInfoVector() { }
    void insert(std::string f, double v) {
        data_.push_back(RefSeqInfo(f, v));
    }
   RefSeqInfo& at(int ix) { return data_.at(ix); } 
   int find(const char* f) {
       std::vector<RefSeqInfo>::const_iterator it = std::lower_bound(data_.begin(), data_.end(), RefSeqInfo(f, 0));
       if (it == data_.end() || it->str != f) { return -1; }
       return it - data_.begin();
   }
   void sort() { std::sort(data_.begin(), data_.end()); }
   unsigned size() const { return data_.size(); }
   std::vector<RefSeqInfo> data_;
};

extern "C" {
    void* rsiv_free(void* p) { delete static_cast<RefSeqInfoVector*>(p); }
}

