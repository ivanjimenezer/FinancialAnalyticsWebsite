-- MySQL dump 10.13  Distrib 8.0.34, for Win64 (x86_64)
--
-- Host: localhost    Database: anfin
-- ------------------------------------------------------
-- Server version	8.0.35

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `dollar_prices`
--

DROP TABLE IF EXISTS `dollar_prices`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `dollar_prices` (
  `id` int NOT NULL AUTO_INCREMENT,
  `date` date NOT NULL,
  `price` decimal(10,2) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=174 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `dollar_prices`
--

LOCK TABLES `dollar_prices` WRITE;
/*!40000 ALTER TABLE `dollar_prices` DISABLE KEYS */;
INSERT INTO `dollar_prices` VALUES (1,'2023-04-07',18.23),(2,'2023-04-10',18.13),(3,'2023-04-11',18.14),(4,'2023-04-12',18.16),(5,'2023-04-13',18.06),(6,'2023-04-14',18.02),(7,'2023-04-17',17.99),(8,'2023-04-18',18.03),(9,'2023-04-19',18.04),(10,'2023-04-20',18.04),(11,'2023-04-21',18.00),(12,'2023-04-24',18.00),(13,'2023-04-25',17.97),(14,'2023-04-26',18.04),(15,'2023-04-27',18.16),(16,'2023-04-28',18.02),(17,'2023-05-01',17.99),(18,'2023-05-02',17.94),(19,'2023-05-03',17.97),(20,'2023-05-04',17.96),(21,'2023-05-05',17.90),(22,'2023-05-08',17.77),(23,'2023-05-09',17.79),(24,'2023-05-10',17.74),(25,'2023-05-11',17.54),(26,'2023-05-12',17.57),(27,'2023-05-15',17.60),(28,'2023-05-16',17.43),(29,'2023-05-17',17.48),(30,'2023-05-18',17.61),(31,'2023-05-19',17.72),(32,'2023-05-22',17.75),(33,'2023-05-23',17.88),(34,'2023-05-24',17.96),(35,'2023-05-25',17.80),(36,'2023-05-26',17.85),(37,'2023-05-29',17.60),(38,'2023-05-30',17.60),(39,'2023-05-31',17.66),(40,'2023-06-01',17.68),(41,'2023-06-02',17.54),(42,'2023-06-05',17.55),(43,'2023-06-06',17.45),(44,'2023-06-07',17.37),(45,'2023-06-08',17.36),(46,'2023-06-09',17.37),(47,'2023-06-12',17.27),(48,'2023-06-13',17.28),(49,'2023-06-14',17.23),(50,'2023-06-15',17.13),(51,'2023-06-16',17.13),(52,'2023-06-19',17.07),(53,'2023-06-20',17.07),(54,'2023-06-21',17.20),(55,'2023-06-22',17.12),(56,'2023-06-23',17.17),(57,'2023-06-26',17.15),(58,'2023-06-27',17.12),(59,'2023-06-28',17.08),(60,'2023-06-29',17.08),(61,'2023-06-30',17.11),(62,'2023-07-03',17.12),(63,'2023-07-04',17.05),(64,'2023-07-05',17.04),(65,'2023-07-06',17.01),(66,'2023-07-07',17.23),(67,'2023-07-10',17.15),(68,'2023-07-11',17.04),(69,'2023-07-12',17.03),(70,'2023-07-13',16.90),(71,'2023-07-14',16.85),(72,'2023-07-17',16.74),(73,'2023-07-18',16.71),(74,'2023-07-19',16.75),(75,'2023-07-20',16.71),(76,'2023-07-21',16.87),(77,'2023-07-24',16.97),(78,'2023-07-25',16.83),(79,'2023-07-26',16.93),(80,'2023-07-27',16.86),(81,'2023-07-28',16.85),(82,'2023-07-31',16.68),(83,'2023-08-01',16.74),(84,'2023-08-02',16.87),(85,'2023-08-03',17.00),(86,'2023-08-04',17.32),(87,'2023-08-07',17.08),(88,'2023-08-08',17.06),(89,'2023-08-09',17.11),(90,'2023-08-10',17.06),(91,'2023-08-11',17.07),(92,'2023-08-14',16.99),(93,'2023-08-15',17.05),(94,'2023-08-16',17.13),(95,'2023-08-17',17.14),(96,'2023-08-18',17.10),(97,'2023-08-21',17.03),(98,'2023-08-22',17.01),(99,'2023-08-23',16.91),(100,'2023-08-24',16.78),(101,'2023-08-25',16.83),(102,'2023-08-28',16.72),(103,'2023-08-29',16.79),(104,'2023-08-30',16.79),(105,'2023-08-31',16.76),(106,'2023-09-01',17.00),(107,'2023-09-04',17.07),(108,'2023-09-05',17.18),(109,'2023-09-06',17.38),(110,'2023-09-07',17.58),(111,'2023-09-08',17.58),(112,'2023-09-11',17.54),(113,'2023-09-12',17.28),(114,'2023-09-13',17.21),(115,'2023-09-14',17.13),(116,'2023-09-15',17.11),(117,'2023-09-18',17.05),(118,'2023-09-19',17.12),(119,'2023-09-20',17.06),(120,'2023-09-21',17.09),(121,'2023-09-22',17.21),(122,'2023-09-25',17.17),(123,'2023-09-26',17.38),(124,'2023-09-27',17.55),(125,'2023-09-28',17.69),(126,'2023-09-29',17.54),(127,'2023-10-02',17.40),(128,'2023-10-03',17.66),(129,'2023-10-04',18.01),(130,'2023-10-05',17.98),(131,'2023-10-06',18.28),(132,'2023-10-09',18.23),(133,'2023-10-10',18.21),(134,'2023-10-11',17.93),(135,'2023-10-12',17.84),(136,'2023-10-13',17.97),(137,'2023-10-16',18.05),(138,'2023-10-17',17.91),(139,'2023-10-18',18.00),(140,'2023-10-19',18.24),(141,'2023-10-20',18.31),(142,'2023-10-23',18.20),(143,'2023-10-24',18.13),(144,'2023-10-25',18.26),(145,'2023-10-26',18.33),(146,'2023-10-27',18.14),(147,'2023-10-30',18.11),(148,'2023-10-31',18.05),(149,'2023-11-01',18.05),(150,'2023-11-02',17.76),(151,'2023-11-03',17.53),(152,'2023-11-06',17.47),(153,'2023-11-07',17.54),(154,'2023-11-08',17.47),(155,'2023-11-09',17.53),(156,'2023-11-10',17.79),(157,'2023-11-13',17.66),(158,'2023-11-14',17.60),(159,'2023-11-15',17.33),(160,'2023-11-16',17.30),(161,'2023-11-17',17.23),(162,'2023-11-20',17.22),(163,'2023-11-21',17.11),(164,'2023-11-22',17.19),(165,'2023-11-23',17.20),(166,'2023-11-24',17.19),(167,'2023-11-27',17.11),(168,'2023-11-28',17.16),(169,'2023-11-29',17.12),(170,'2023-11-30',17.28),(171,'2023-12-01',17.38),(172,'2023-12-04',17.19),(173,'2023-12-05',17.45);
/*!40000 ALTER TABLE `dollar_prices` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2023-12-07  0:07:26