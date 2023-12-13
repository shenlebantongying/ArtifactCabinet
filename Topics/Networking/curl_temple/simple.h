#ifndef CPP_NETWORKING_SIMPLE_H
#define CPP_NETWORKING_SIMPLE_H

bool curl_configure();
bool download_file(const char url[],const char out_path[]);
void curl_close();

#endif //CPP_NETWORKING_SIMPLE_H
