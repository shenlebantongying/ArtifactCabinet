#include <curl/curl.h>
#include <cstdio>
#include <curl/easy.h>
#include <cstring>

//Goal: download https://github.com/torvalds/linux/archive/master.zip

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written;
    written = fwrite(ptr, size, nmemb, stream);
    return written;
}

CURL *curl;
CURLcode res;

bool curl_configure(){

    curl_version_info_data * vinfo = curl_version_info(CURLVERSION_NOW);

    if(vinfo->features & CURL_VERSION_SSL){
        printf("CURL: SSL enabled\n");
    }else{
        printf("CURL: SSL not enabled\n");
    }
    curl = curl_easy_init();
    //Follow redirections
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    //Custom write function
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

    if(curl){
        return true;
    } else {
        return false;
    }
}

bool download_file(const char url[],const char out_path[]){

    FILE *fp = fopen(out_path, "wb");;

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);

    res = curl_easy_perform(curl);
    fclose(fp);
}

void curl_close(){
    curl_easy_cleanup(curl);
}


int main() {

    const char url[] = "https://github.com/torvalds/linux/archive/master.zip";
    const char out[FILENAME_MAX] = "./linux.zip";

    curl_configure();
    download_file(url,out);
    curl_close();

    return 0;
}
