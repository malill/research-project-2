{
 filter: {
  name: {
   $ne: 'order_entry'
  },
  path: {
   $ne: 'https://fanshop.fk-austria.at/dmlcomstorefront/dmlcom/de/'
  }
 },
 sort: {
  timestamp: 1
 }
}