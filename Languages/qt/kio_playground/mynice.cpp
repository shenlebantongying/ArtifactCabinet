#include <KIO/ListJob>
#include <QUrl>
#include <QDir>
#include <iostream>

#include "mynice.h"

void mynice::test(){
    QUrl u = QUrl::fromLocalFile("file:/home/");

    KIO::ListJob * listJob = KIO::listDir(u.path(),KIO::HideProgressInfo);
    listJob->setUiDelegate(nullptr);

    QObject::connect(listJob,&KIO::ListJob::entries,[=](KIO::Job*, const KIO::UDSEntryList& list){
        std::cout<<list.count()<<std::endl;;

        for(const auto & it : list){
            QString name= it.stringValue(KIO::UDSEntry::UDS_NAME);
            std::cout<<name.toStdString()<<std::endl;
        }

    });

    connect(listJob, &KJob::result, this, [=](KJob *job) {
    });

}