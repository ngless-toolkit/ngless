#include <vector>
#include <string>
#include <cstring>
#include <algorithm>

const unsigned block_size = 8192 - 64; // 64 is for padding
struct PoolC {
    PoolC():free_(0) { }
    ~PoolC() {
        for (std::vector<void*>::iterator it = data_.begin(), past = data_.end(); it != past; ++it) {
            operator delete(*it);
        }
    }
    const char* strdup(const char* input) {
        const int len = std::strlen(input) + 1;
        if (len > block_size) {
            char* n = static_cast<char*>(operator new(len));
            std::strcpy(n, input);
            data_.push_back(n);
            free_ = 0;
            return n;
        }
        if (len > free_) {
            data_.push_back(operator new(block_size));
            free_ = block_size;
        }
        char* r = static_cast<char*>(data_.back());
        r += block_size - free_;
        std::strcpy(r, input);
        free_ -= len;
        return r;
    }
    private:

    std::vector<void*> data_;
    int free_;
};


struct RefSeqInfo {
    RefSeqInfo(const char* s, double v)
        :str(s)
        ,val(v) { }
    bool operator <(const RefSeqInfo& rhs) const { return strcmp(str, rhs.str) < 0; }
    const char* str;
    double val;
};


struct RefSeqInfoVector {
    RefSeqInfoVector() { }
    ~RefSeqInfoVector() { }
    void insert(std::string f, double v) {
        data_.push_back(RefSeqInfo(pool_.strdup(f.c_str()), v));
    }
   RefSeqInfo& at(int ix) { return data_.at(ix); }
   int find(const char* f) {
       std::vector<RefSeqInfo>::const_iterator it = std::lower_bound(data_.begin(), data_.end(), RefSeqInfo(f, 0));
       if (it == data_.end() || strcmp(it->str, f)) { return -1; }
       return it - data_.begin();
   }
   void sort() { std::sort(data_.begin(), data_.end()); }
   unsigned size() const { return data_.size(); }
   std::vector<RefSeqInfo> data_;
   PoolC pool_;
};

extern "C" {
    void* rsiv_free(void* p) { delete static_cast<RefSeqInfoVector*>(p); }
}

