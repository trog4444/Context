namespace Directories


module Dropbox =

    [<Literal>]
    let DATETIME_FORMAT = @"yyyy-MM-dd hh:mm:ss:xxxx"

    type DateTime = string  // placeholder.

    type Dates =
        { Creation:   DateTime
          LastUpdate: DateTime }

    type Property = string  // placeholder.

    type File =
        { Name:  string
          Ext:   string
          Dates: Dates
          Props: Set<Property> }


    type FolderName = string    // placeholder.
    type FolderInfo = string    // placeholder.
    type FileName = string  // placeholder.

    type Folder =
        { Name:   FileName
          Parent: Folder option
          Info:   FolderInfo
          Files:  Set<File>
          Subs:   Set<Folder> }

    type LazyFolder =
        { Name:   FolderName
          Parent: LazyFolder option
          Info:   FolderInfo
          Files:  Map<FileName, Lazy<File>>
          Subs:   Map<FolderName, Lazy<LazyFolder>> }



